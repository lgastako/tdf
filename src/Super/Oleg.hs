{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wall                        #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports          #-}

-- http://oleg.fi/gists/posts/2019-07-15-fancy-types-for-cassava.html
-- (via https://target.github.io/row-types/examples/RowCSV.html)

module Super.Oleg where

import           Control.Monad           ( forM )
import           Data.Bifunctor          ( first
                                         , bimap
                                         )
import           Data.Kind               ( Type )
import           Data.Tagged             ( Tagged(..) )
import           Data.Text               ( Text )
import           Data.Type.Equality      ( (:~:)(..) )
import           Data.Fin                ( Fin(..) )
import           Data.Type.Nat           ( Nat(..)
                                         , SNatI
                                         )
import           Data.Vec.Lazy           ( Vec(..) )
import           GHC.Generics            -- ( (:*:)(..)
                                         -- , Generic
                                         -- , Rep
                                         -- , D1
                                         -- , K1( K1 )
                                         -- , M1( M1 )
                                         -- , U1
                                         -- , from
                                         -- )
import           Text.Read               ( readMaybe )

import qualified Data.Fin           as F
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.Type.Nat      as N
import qualified Data.Vec.Lazy      as V

data PL = PL
  { plName   :: Text
  , plYear   :: Int
  , plPerson :: Text
  } deriving (Eq, Generic, Ord, Show)

pls :: [PL]
pls =
  [ PL "Haskell" 1990 "Simon"
  , PL "Scala"   2004 "Martin"
  , PL "Idris"   2009 "Edwin"
  , PL "Perl"    1987 "Larry"
  ]

class ToField a where
  toField :: a -> Text

instance ToField Text where
  toField = id

instance ToField Int where
  toField = T.pack . show

-- field count in a record: its size.
type family Size (a :: Type) :: Nat

class ToRecord r where
 toRecord :: r -> Vec (Size r) Text

type GSize a = GSizeF (Rep a) 'Z -- start from zero

type family GSizeF (f :: Type -> Type) (acc :: Nat) :: Nat where
    GSizeF U1         acc = acc
    GSizeF (K1 i a)   acc = 'S acc
    GSizeF (M1 i c f) acc = GSizeF f acc
    GSizeF (f :*: g)  acc = GSizeF f (GSizeF g acc)

type instance Size PL = GSize PL

-- Also we can check this dependent-language style. If this test fails, it will
-- be a compilation error. This definitely blurs the distiction between tests
-- and types :)
check1 :: Size PL :~: N.Nat3
check1 = Refl

genericToRecord
    :: forall r. (Generic r, GToRecord (Rep r))
    => r -> Vec (GSize r) Text
genericToRecord = gtoRecord VNil . from

class GToRecord rep where
  gtoRecord :: Vec acc Text -> rep () -> Vec (GSizeF rep acc) Text

instance GToRecord U1 where
  gtoRecord xs _ = xs

instance ToField c => GToRecord (K1 i c) where
  gtoRecord xs (K1 c) = toField c ::: xs

instance GToRecord f => GToRecord (M1 i c f) where
  gtoRecord xs (M1 f) = gtoRecord xs f

instance (GToRecord f, GToRecord g) => GToRecord (f :*: g) where
  gtoRecord xs (f :*: g) = gtoRecord (gtoRecord xs g) f

instance ToRecord PL where
  toRecord = genericToRecord

class Header r where
  header :: Tagged r (Vec (Size r) Text)

-- We could write generic implementation for it, but as dealing with metadata
-- in GHC.Generics is not pretty, I'll implement PL instance manually:

instance Header PL where
  header = Tagged $ "name" ::: "year" ::: "person" ::: VNil

encode :: forall r. (Header r, ToRecord r) => [r] -> Text
encode rs = T.unlines $ map (T.intercalate "," . V.toList)
  $ unTagged (header :: Tagged r _)
  : map toRecord rs

run :: IO ()
run = putStr . T.unpack $ encode pls

type Error = String

class FromField a where
  fromField :: Text -> Either String a

instance FromField Text where
  fromField = Right

instance FromField Int where
  fromField t
    = maybe (Left $ "Invalid Int: " ++ show t) Right
    $ readMaybe $ T.unpack t

class FromRecord r where
  fromRecord :: Vec (Size r) Text -> Either Error r

genericFromRecord
    :: forall r. (Generic r, GFromRecord (Rep r))
    => Vec (GSize r) Text -> Either String r
genericFromRecord ts =
    let tmp :: Either Error (Rep r (), Vec 'Z Text)
        tmp = gfromRecord ts
    in to . fst <$> tmp

class GFromRecord rep where
  gfromRecord :: Vec (GSizeF rep acc) Text
              -> Either Error (rep (), Vec acc Text)

instance GFromRecord U1 where
  gfromRecord xs = pure (U1, xs)

instance FromField c => GFromRecord (K1 i c) where
  gfromRecord (x ::: xs) = do
    y <- fromField x
    pure (K1 y, xs)

instance GFromRecord f => GFromRecord (M1 i c f) where
  gfromRecord = fmap (first M1) . gfromRecord

instance (GFromRecord f, GFromRecord g) => GFromRecord (f :*: g) where
  gfromRecord xs = do
    (f, xs')  <- gfromRecord xs
    (g, xs'') <- gfromRecord xs'
    pure (f :*: g, xs'')

instance FromRecord PL where
  fromRecord = genericFromRecord

input :: Text
input = T.unlines
  [ "year,name,types,person,website"
  , "1987,Perl,no,Larry,https://www.perl.org/"
  , "1990,Haskell,nice,Simon,https://www.haskell.org/"
  , "2004,Scala,weird,Martin,https://www.scala-lang.org/"
  , "2009,Idris,fancy,Edwin,https://www.idris-lang.org/"
  ]

prepare :: Text -> Either Error ([Text], [[Text]])
prepare i = case map (T.splitOn ",") (T.lines i) of
  []     -> Left "No header"
  (r:rs) -> Right (r, rs)

data Extract :: Nat -> Nat -> Type where
    Step :: Fin ('S n)             -- take a nth value, x
         -> Extract n m            -- recursively extract rest, xs
         -> Extract ('S n) ('S m)  -- cons x xs
    Done :: Extract n 'Z           -- or we are done.

deriving instance Show (Extract n m)

extract :: Extract n m -> Vec n a -> Vec m a
extract Done       _  = VNil
extract (Step n e) xs = case delete n xs of
  (x, xs') -> x ::: extract e xs'

delete :: Fin ('S n) -> Vec ('S n) a -> (a, Vec n a)
delete FZ           (x ::: xs)       = (x, xs)
delete (FS FZ)      (x ::: y ::: xs) = (y, x ::: xs)
delete (FS n@FS {}) (x ::: xs)       = case delete n  xs of
  (y, ys) -> (y, x ::: ys)

columns
    :: (Eq a, Show a)
    => Vec m a         -- ^ wanted header values
    -> Vec n a         -- ^ given header values
    -> Either Error (Extract n m)
columns VNil       _            = Right Done
columns (_ ::: _)  VNil         = Left "not enought header values"
columns (h ::: hs) xs@(_ ::: _) = do
    (n, xs') <- find' h xs  -- find first value
    rest <- columns hs xs'  -- recurse
    pure $ Step n rest      -- record the trace

find'
    :: (Eq a, Show a)
    => a
    -> Vec ('S n) a
    -> Either Error (Fin ('S n), Vec n a)
find' x (y ::: ys)
  | x == y    = Right (FZ, ys)
  | otherwise = case ys of
      VNil    -> Left $ "Cannot find header value " ++ show x
      _ ::: _ -> do
        (n, zs) <- find' x ys
        pure (FS n, y ::: zs)

-- -- Reify any list [a] to Vec n a.
-- reifyList :: [a] -> (forall n. SNatI n => Vec n a -> r) -> r

-- -- Convert list [a] to Vec n a. Returns Nothing if input list is too short.
-- fromListPrefix :: SNatI n => [a] -> Maybe (Vec n a)

decode :: forall r. (Header r, FromRecord r) => Text -> Either String [r]
decode contents = do
  (hs,xss) <- prepare contents
  V.reifyList hs $ \hs' -> do
    trc <- columns (unTagged (header :: Tagged r _)) hs'
    forM xss $ \xs -> do
      xs' <- maybe (Left "not enough columns") Right
               $ V.fromListPrefix xs
      fromRecord (extract trc xs')

main :: IO ()
main = case decode input :: Either String [PL] of
  Left err -> putStrLn $ "ERROR: " ++ err
  Right xs -> mapM_ print xs
