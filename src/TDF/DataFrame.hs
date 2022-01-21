{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TDF.DataFrame
  ( DataFrame
  , Options(..)
  , ToField(..)
  , Verbosity(..)
  , at
  , columns
  , construct
  , empty
  , fromList
  , fromNativeVector
  , fromScalarList
  , fromVector
  , getColumn
  , head
  , head_
  , index
  , info
  , isEmpty
  , map
  , memSize
  , ndims
  , ncols
  , nrows
  , onColumn
  , onVec
  , opts
  , over_
  , reindex
  , renderWith
  , shape
  , size
  , tail
  , tail_
  , toList
  , toNativeVector
  , toVector
  , under_
  ) where

import           TDF.Prelude         hiding ( empty
                                            , head
                                            , map
                                            , toList
                                            )

import           Control.DeepSeq            ( ($!!)
                                            , NFData
                                            )
import qualified Data.List        as List
import qualified Data.Row.Records as Rec
import qualified Data.Text        as T
import qualified Data.Vector      as Vector
import qualified GHC.DataSize     as Data

data DataFrame idx a = DataFrame
  { dfIndexes :: [idx]
  , dfData    :: Rec (Map Vector a)
  , dfLength  :: Int
  } deriving (Generic)

instance (Forall (Map Vector a)  NFData, NFData idx)
  => NFData (DataFrame idx a)

-- TODO: not the representaton we want but fine for Show... need
--       "real" functions  for rendering
deriving instance ( Forall (Map Vector a) Show
                  , Show idx
                  ) => Show (DataFrame idx a)

deriving instance ( Forall (Map Vector a) Eq
                  , Eq idx
                  ) => Eq (DataFrame idx a)

data Options idx a = Options
  { optIndexes :: [idx]
  , optData    :: Vector (Rec a)
  }

class ToField a where
  toField :: a -> Text

instance ToField Text where
  toField = identity

instance ToField Integer where
  toField = T.pack . show

instance ToField Int where
  toField = T.pack . show

instance ToField String where
  toField = T.pack

data Verbosity
  = Quiet
  | Verbose
  deriving (Bounded, Enum, Generic, Eq, Ord, Show)

-- TODO find better name for this so as to not conflict with "real" RangeIndex
type RangeIndex idx = (Int, Maybe (idx, idx))
type ColIndex       = (Int, Maybe (String, String))

opts :: Options Int a
opts = Options
  { optIndexes = [0..]
  , optData    = Vector.empty
  }

-- https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.html#pandas.DataFrame
construct :: forall idx a.
             Forall a Unconstrained1
          => Options idx a
          -> DataFrame idx a
construct Options {..} = DataFrame optIndexes d (Vector.length optData)
  where
    _ = optData :: Vector (Rec a)

    d :: Forall a Unconstrained1 => Rec (Map Vector a)
    d = Rec.distribute optData

-- | The index (row labels) of the DataFrame.
index :: DataFrame idx a -> [idx]
index DataFrame {..} = List.take n dfIndexes
  where
    n = panic "index: Vector.length dfData"

-- TODO: questionable to expose? at least with this interface... doing it for
--       now for Examples.hs
reindex :: [idx'] -> DataFrame idx a -> DataFrame idx' a
reindex dfIndexes' DataFrame {..} = DataFrame dfIndexes' dfData dfLength

columns :: forall idx a. (Forall a ToField) => DataFrame idx a -> [String]
columns _ = Rec.labels @a @ToField

-- TODO: dtypes
-- TODO: select_dtypes


-- TODO: axes ?
-- data Axes
-- axes :: DataFrame idx a -> Axes
-- axes = error "axes not implemented"

-- TODO: Can we do away with the `Forall a ToField` stuff by substituting
-- a contcreate ToField function from row-types?

ncols :: Forall a ToField => DataFrame idx a -> Int
ncols = length . columns

-- I think this is right?
ndims :: Forall a ToField => DataFrame idx a -> Int
ndims df
  | length (columns df) <= 1 = 1
  | otherwise                = 2

nrows :: DataFrame idx a -> Int
nrows DataFrame {..} = dfLength

-- TODO use "Renderable" or something instead of Show for the idxs

info :: (Forall a ToField, Show idx)
     => Verbosity
     -> DataFrame idx a
     -> String
info verbosity = case verbosity of
  Quiet   -> infoQuiet
  Verbose -> infoVerbose

infoQuiet :: (Forall a ToField, Show idx)
          => DataFrame idx a
          -> String
infoQuiet df = List.unlines
  [ showRangeIndex (rangeIndex df)
  , showColIndex (colIndex df)
  ]

infoVerbose :: Show idx => DataFrame idx a -> String
infoVerbose df = List.unlines
  [ showRangeIndex (rangeIndex df) ++ "\nmore soon\n"
  , "more soon (verbose)"
  ]

-- TODO: this is poorly named, especially since we're probably about to implement
--       our own "real" RangeIndex type.
rangeIndex :: DataFrame idx a -> RangeIndex idx
rangeIndex df = ( length idxs
                , case idxs of
                    []    -> Nothing
                    (f:xs) -> case reverse xs of
                                [] -> Nothing
                                (l:_) -> Just (f, l)
                )
  where
    idxs = index df

showRangeIndex :: Show idx => RangeIndex idx -> String
showRangeIndex (n, Nothing)     = "Range index: " <> show n <> " entries."
showRangeIndex (n, Just (f, l)) = "Range index: " <> show n <> " entries, "
                               <> show f <> " to " <> show l

-- TODO truncate indexes on construction so this doesn't infintie loop
memSize :: ( Forall (Map Vector a) NFData
           , MonadIO m
           , NFData idx
           )
        => DataFrame idx a
        -> m Word
memSize = (liftIO . Data.recursiveSize $!!)

colIndex :: Forall a ToField => DataFrame idx a -> ColIndex
colIndex df = ( length cols
              , case cols of
                  []    -> Nothing
                  (f:xs) -> case reverse xs of
                              [] -> Nothing
                              (l:_) -> Just (f, l)
              )
  where
    cols = columns df

showColIndex :: ColIndex -> String
showColIndex (n, Nothing)     = "Columns: " <> show n <> " entries."
showColIndex (n, Just (f, l)) = "Columns: " <> show n <> " entries, "
                             <> show f <> " to " <> show l

fromList :: Forall a Unconstrained1 => [Rec a] -> DataFrame Int a
fromList = fromVector . Vector.fromList

fromScalarList :: [a] -> DataFrame Int ("value" .== a)
fromScalarList = fromList . List.map (\x -> #value .== x)

fromVector :: Forall a Unconstrained1
           => Vector (Rec a)
           -> DataFrame Int a
fromVector recs = construct opts
  { optData    = recs
  , optIndexes = [0.. Vector.length recs]
  }

onVec :: forall idx a b.
         ( Forall a Unconstrained1
         , Forall b Unconstrained1
         )
      => (Vector (Rec a) -> Vector (Rec b))
      -> DataFrame idx a
      -> DataFrame idx b
onVec f DataFrame {..} = DataFrame
  { dfIndexes = dfIndexes
  , dfData    = dfData'
  , dfLength  = dfLength'
  }
  where
    _ = dfData :: Rec (Map Vector a)
    (dfData', dfLength') = ((,) <$> Rec.distribute <*> Vector.length)
                           . f
                           . Rec.sequence
                           $ dfData

over_ :: forall a b.
         ( Forall a Unconstrained1
         , Forall b Unconstrained1
         )
      => (Rec a -> Rec b)
      -> Rec (Map Vector a)
      -> Rec (Map Vector b)
over_ f = under_ (Vector.map f)

under_ :: forall a b.
         ( Forall a Unconstrained1
         , Forall b Unconstrained1
         )
      => (Vector (Rec a) -> Vector (Rec b))
      -> Rec (Map Vector a)
      -> Rec (Map Vector b)
under_ f = Rec.distribute . f . Rec.sequence

-- onVec f (DataFrame idx v len) = DataFrame idx (mapify f v) len
--   where
--     mapify :: (Vector (Rec a) -> Vector (Rec b))
--            -> Rec (Map Vector a)
--            -> Rec (Map Vector b)
--     mapify = undefined

map :: ( Forall a Unconstrained1
       , Forall b Unconstrained1
       )
    => (Rec a -> Rec b)
    -> DataFrame idx a
    -> DataFrame idx b
map f = onVec (Vector.map f)

-- column :: ( KnownSymbol label
--           , KnownNat (RecSize a)
--           , KnownNat ((RecSize a - RecTyIdxH 0 label a) - 1)
--           )
--        => FldProxy label
--        -> DataFrame idx a
--        -> DataFrame idx _
-- --     -> DataFrame '[label := SuperRecord.RecTy label a]
-- column label = map (relabel label)

-- relabel :: ( KnownSymbol l
--            , KnownNat (RecSize lts)
--            , KnownNat ((RecSize lts - RecTyIdxH 0 l lts) - 1))
--         => FldProxy l
--         -> Rec lts
--         -> Rec _
-- --      -> Rec '[l := SuperRecord.RecTy l lts]
-- relabel :: _ -> a ->
-- relabel :: Getter s a -> Rec _ -> Rec _
_relabel :: forall r k v rest.
            ( KnownSymbol k
            , r ≈ k .== v
            , Disjoint r rest
            )
         => Label k
         -> Rec (r .+ rest)
         -> Rec ("value" .== v)
_relabel label x = label' .== x .! label
  where
    label' :: Label "value"
    label' = panic "value"

renderWith :: Forall a Unconstrained1
           => (Rec a -> [String])
           -> DataFrame idx a
           -> String
renderWith f (DataFrame _idx v _len) = renderStrings headers rows
  where
    headers :: [String]
    headers = ["TODO", "fix", "column", "names"]

    rows :: Vector [String]
    rows = Vector.map f (Rec.sequence v)

renderStrings :: [String] -> Vector [String] -> String
renderStrings headers rows = List.unlines $
  [top, headerRow, middle] ++ renderedRows ++ [ bottom ]
  where
    renderedRows :: [String]
    renderedRows = Vector.toList $ Vector.map wrap rows

    headerRow :: String
    headerRow = wrap headers

    top, middle, bottom :: String
    top    = lineWith ('+', '-', 'v', '+')
    middle = lineWith ('+', '-', '+', '+')
    bottom = lineWith ('+', '-', '^', '+')

    wrap :: [String] -> String
    wrap = ("| " ++)
      . (++ " |")
      . List.intercalate " | "
      . zipWith pad maxWidths

    pad :: Int -> String -> String
    pad n s = replicate (length s - n) ' ' ++ s

    maxWidths :: [Int]
    maxWidths = fmap maximum
              . List.transpose
              . fmap (fmap length)
              $ allStrings

    lineWith :: (Char, Char, Char, Char) -> String
    lineWith (left, _mid, _break, _right) = left:rest
      where
        rest = "[REST]"

    allStrings :: [[String]]
    allStrings = headers:rowStrings

    rowStrings :: [[String]]
    rowStrings = Vector.toList rows

shape :: Forall a ToField => DataFrame idx a -> (Int, Int)
shape = (,) <$> nrows <*> ncols

size :: Forall a ToField => DataFrame idx a -> Int
size = (*) <$> ncols <*> nrows

toList :: Forall a Unconstrained1 => DataFrame idx a -> [Rec a]
toList = Vector.toList . toVector

-- values replaced by toVector
-- to_numpy replaced by toVector
toVector :: Forall a Unconstrained1 => DataFrame idx a -> Vector (Rec a)
toVector DataFrame {..} = Rec.sequence dfData

isEmpty :: Forall a ToField => DataFrame idx a -> Bool
isEmpty df
  | ncols df == 0 = True
  | nrows df == 0 = True
  | otherwise     = False

empty :: DataFrame () Empty
empty = construct $ Options
  { optIndexes = [] :: [()]
  , optData = Vector.empty
  }

head :: forall idx a.
        Forall a Unconstrained1
     => Int
     -> DataFrame idx a
     -> DataFrame idx a
head n df@DataFrame {..} = DataFrame
  { dfIndexes = List.take   m dfIndexes
  , dfData    = Rec.distribute . f . Rec.sequence $ dfData
  , dfLength  = dfLength
  }
  where
    f :: Forall a Unconstrained1 => Vector (Rec a) -> Vector (Rec a)
    f = Vector.take m

    m | n > 0     = n
      | otherwise = nrows df + n

head_ :: Forall a Unconstrained1 => DataFrame idx a -> DataFrame idx a
head_ = head 5

tail :: forall idx a.
        Forall a Unconstrained1
     => Int
     -> DataFrame idx a
     -> DataFrame idx a
tail n df@DataFrame {..} = DataFrame
  { dfIndexes = List.drop   m dfIndexes
  , dfData    = Rec.distribute . f . Rec.sequence $ dfData
  , dfLength  = dfLength
  }
  where
    f :: Forall a Unconstrained1 => Vector (Rec a) -> Vector (Rec a)
    f = Vector.drop m

    m | n >= 0    = nrows df - n
      | otherwise = negate n

tail_ :: Forall a Unconstrained1 => DataFrame idx a -> DataFrame idx a
tail_ = tail 5

-- -- TODO: bool

-- TODO we should really at least eliminate the `Forall a ToField` constraint
-- on things that are just getting the column count -- should be able to get
-- that without rendering them.
at :: ( Eq idx
      , Forall a Unconstrained1
      , KnownSymbol k
      )
   => idx
   -> Label k
   -> DataFrame idx a
   -> Maybe (a .! k)
at idx k df = (.! k) <$> lookup idx df

-- TODO: iat -- maybe?

-- TODO this obviously needs to be SOOO much better
lookup :: (Eq idx, Forall a Unconstrained1)
       => idx
       -> DataFrame idx a
       -> Maybe (Rec a)
lookup k DataFrame {..} = fmap snd . Vector.find ((== k) . fst) $ indexed
  where
    dfData' = Rec.sequence dfData
    indexed = Vector.zip (Vector.fromList dfIndexes) dfData'

fromNativeVector :: forall t.
                    ( Rec.FromNative t
                    , Forall (Rec.NativeRow t) Unconstrained1
                    )
                 => Vector t
                 -> DataFrame Int (Rec.NativeRow t)
fromNativeVector values = DataFrame
  { dfData    = Rec.distribute recs
  , dfIndexes = [0 .. Vector.length recs]
  , dfLength  = Vector.length recs
  }
  where
    recs :: Vector (Rec (Rec.NativeRow t))
    recs = Vector.map Rec.fromNative values

toNativeVector :: forall idx t.
                  ( ToNative t
                  , Forall (NativeRow t) Unconstrained1
                  )
               => DataFrame idx (NativeRow t)
               -> Vector t
toNativeVector df@DataFrame {..} = Vector.map Rec.toNative . toVector $ df

-- TODO
-- locLabel :: label -> DataFrame idx a
-- locLabel = undefined

-- project :: [forall lbl. Label lbl] -> DataFrame idx a -> DataFrame idx b
-- project = undefined

-- data ColumnSlice idx
--   = UpTo idx
--   | Through (idx, idx)
--   | Stepped (idx, idx, idx)
--   deriving (Eq, Generic, Ord, Show)

onColumn :: forall k idx a b.
            ( KnownSymbol k
            , (Map Vector a .! k) ~ Vector (a .! k)
            )
         => Label k
         -> (Vector (a Rec..! k) -> b)
         -> DataFrame idx a
         -> b
onColumn k f DataFrame {..} = f $ dfData .! k

getColumn :: forall k idx a.
             ( KnownSymbol k
             , (Map Vector a .! k) ~ Vector (a .! k)
             )
          => Label k
          -> DataFrame idx a
          -> Vector (a .! k)
getColumn = flip onColumn identity
