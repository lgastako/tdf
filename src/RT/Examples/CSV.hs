{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module RT.Examples.CSV where



import           Data.Row.Records        ( Forall
                                         , Rec
                                         , type (.+)
                                         , type (.==)
                                         )
import qualified Data.Row.Records as Rec
import           Data.Text               ( Text )
import qualified Data.Text        as T
import           GHC.Generics            ( Generic )
import           RT.DataFrame            ( ToField( toField ) )

data PL = PL
  { name   :: Text
  , year   :: Int
  , person :: Text
  } deriving (Eq, Ord, Show, Generic)

pls :: [PL]
pls =
    [ PL "Haskell" 1990 "Simon"
    , PL "Scala"   2004 "Martin"
    , PL "Idris"   2009 "Edwin"
    , PL "Perl"    1987 "Larry"
    ]

type PlFields = ("name" .== Text .+ "person" .== Text .+ "year" .== Int)
type PlRec    = Rec PlFields

plrs :: [PlRec]
plrs = Rec.fromNative <$> pls

plr1 :: PlRec
plr1 = head plrs

input :: Text
input = T.unlines
    [ "year,name,types,person,website"
    , "1987,Perl,no,Larry"
    , "1990,Haskell,nice,Simon,https://www.haskell.org/"
    , "2004,Scala,weird,Martin,https://www.scala-lang.org/"
    , "2009,Idris,fancy,Edwin,https://www.idris-lang.org/"
    ]

recToCSV :: forall ρ. Forall ρ ToField => [Rec ρ] -> Text
recToCSV rs = T.unlines $ map (T.intercalate ",")
  $ Rec.labels @ρ @ToField
  : map (Rec.erase @ToField toField) rs

recLabels :: forall a. Forall a ToField => Rec a -> [Text]
recLabels _ = Rec.labels @a @ToField
