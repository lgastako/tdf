{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Fixtures.Examples where

import Data.Frame.Prelude hiding ( Product
                                 , drop
                                 , take
                                 )

import Control.Monad.Cont        ( ContT( ContT )
                                 , runContT
                                 )
import Data.Frame.Typed          ( Axes
                                 , Frame
                                 )
import Data.Frame.Typed.Series   ( Series
                                 , a_
                                 )
import Faker                     ( Fake )
import System.IO.Unsafe          ( unsafePerformIO )

import qualified Data.Frame.Typed         as DF
import qualified Data.Frame.Typed.CSV     as CSV
import qualified Data.Frame.Typed.Index   as Index
import qualified Data.Frame.Typed.Options as Options
import qualified Data.Frame.Typed.Series  as Series
import qualified Data.List                as List
import qualified Data.List.NonEmpty       as NE
import qualified Data.Row.Records         as Rec
import qualified Data.Text                as Text
import qualified Data.Vec.Lazy            as Vec
import qualified Faker.Address            as FAddress
import qualified Faker.Ancient            as FAncient
import qualified Faker.Appliance          as FApp
import qualified Faker.Cannabis           as FCannabis
import qualified Faker.FunnyName          as FFunny
import qualified Faker.Name               as FName
import qualified Faker.Marketing          as FBuzz
import qualified Faker.PhoneNumber        as FPhone

type PersonFields = NameFields .+ AgeFields

type VecPersonFields (n :: Nat) =
  (  "name" .== Vec n Text
  .+ "age"  .== Vec n Int
  )

type    Person   = Rec     PersonFields
type VecPerson n = Rec (VecPersonFields n)

type PlayerFields =
  (  "name" .== Text
  .+ "team" .== Text
  )

type Player = Rec PlayerFields

type AgeFields = "age" .== Int

type AgeRec = Rec AgeFields

type NameFields     = "name" .== Text
type FullNameFields = "fullName" .== Text

type FullPersonFields = FullNameFields .+ AgeFields

type FullPerson = Rec FullPersonFields
type NameRec    = Rec NameFields

data Person' = Person'
  { name :: Text
  , age  :: Int
  } deriving (Generic, Show)

personName1 :: NameRec
personName1 = #name .== "John"

justName :: Person -> NameRec
justName p = #name .== (p .! #name)

person :: Person
person = #name .== "Alex"
      .+ #age .== 23

personTimes100 :: Person
personTimes100 = Rec.update #age 2300 person

person2 :: Person
person2 = #age  .== 45
       .+ #name .== "Dave"

person3 :: Person
person3 = #age  .== 23
       .+ #name .== "Fred"

person2Times100 :: Person
person2Times100 = Rec.update #age 4500 person2

greet :: (r ≈ "name" .== Text)
      => Rec r
      -> Text
greet = ("Hello " <>) . (.! #name)

df1 :: Frame Nat2 Int PersonFields
df1 = DF.fromList
  [ person
  , person2
  ]
  |> orCrash "Examples.df1"

df1Fred :: Frame Nat2 Int PersonFields
df1Fred = DF.fromList
  [ person3
  , person2
  ]
  |> orCrash "Examples.df1"

df1Times100 :: Frame Nat2 Int PersonFields
df1Times100 = DF.fromList
  [ personTimes100
  , person2Times100
  ]
  |> orCrash "Examples.df1Times100"

df1Renamed :: Frame Nat2 Int FullPersonFields
df1Renamed = DF.rename #name #fullName df1

-- TODO can we make it easy to do the same thing with just "age" (or #age) to
--      avoid having to know the type... where the return type is whatever the
--      type of "age" is in in df1?  maybe we don't want to lose the type info?
--      so perhaps DF.restrict replaces both select_types and whatever other
--      subsetting facilities? let's assume so for now.
ages :: Frame Nat2 Int ("age" .== Int)
ages = DF.restrict df1

df1Axes :: Axes Int
df1Axes = DF.axes df1

-- Neat.
df1Restricted :: Frame Nat2 Int NameFields
df1Restricted = DF.restrict df1

-- λ> DF.display $ DF.restrict @NameFields df1
-- name
-- ----
-- Alex
-- Dave

df1' :: Frame Nat2 Int PersonFields
df1' = DF.reindex (Index.fromVec idx') df1
  where
    idx' :: Vec Nat2 Int
    idx' = Vec.fromList [5, 6] `onCrash` "Examples.df1'"

-- λ> DF.memSize df1'
-- 1096
-- Yeesh that's a lot of overhead for [{name:Alex,age:23},{name:Dave,age:45}]

df1'' :: Frame Nat2 Int (PersonFields .+ "foo" .== Text)
df1'' = DF.map (\x -> x .+ #foo .== ("bar" ::Text)) df1'

df2 :: Frame Nat2 Int NameFields
df2 = DF.map justName df1

df3' :: Frame Nat2 Int (PersonFields .+ "fullName" .== Text)
df3' = DF.map plusFull df1

df3 :: Frame Nat2 Int NameFields
df3 = DF.column #name df1

df6 :: Frame Nat3 Int PersonFields
df6 = DF.fromNativeVec nativeVector

nativeVector :: Vec Nat3 Person'
nativeVector = Vec.fromList
  [ Person' "bob"  86
  , Person' "dave" 55
  , Person' "john" 46
  ]
  |> orCrash "nativeVector"

something :: VecPerson Nat3
something = Rec.distribute . DF.toVec $ df6

person's :: [Person']
person's = Vec.toList . DF.toNativeVec $ df6

type DateExampleFields = "year"  .== Integer
                      .+ "month" .== Int
                      .+ "day"   .== Int
                      .+ "value" .== Int

type DateColsLen = Nat6

dateAcrossCols :: Frame DateColsLen Int DateExampleFields
dateAcrossCols = DF.construct
  . Options.fromVec
  . fromMaybe (panic  "Examples.dateCols")
  . Vec.fromList
  . map mkRec
  $ [ (1, 1, 2010, 5)
    , (1, 2, 2010, 6)
    , (1, 3, 2010, 4)
    , (1, 1, 2011, 15)
    , (1, 2, 2011, 17)
    , (1, 3, 2011, 14)
    ]
  where
    mkRec (m, d, y, v) = #year  .== y
                      .+ #month .== m
                      .+ #day   .== d
                      .+ #value .== v

-- Need to move this to a demo of tdf-tools
--
-- -- These same things could be done for Date+Time and other formats.  Ultimately
-- -- I would like to avoid having the core Frame package avoid a dependency
-- -- on the `time` package.  So I'll probably move this out to separate add-on
-- -- lib sooner or later.
-- dateInCol :: Frame DateColsLen Int ("date" .== Day .+ "value" .== Int)
-- dateInCol = dateAcrossCols
--   |> DF.extendWithDay #date
--        ((,,) <$> (.! #year) <*> (.! #month) <*> (.! #day))
--   |> DF.restrict

-- Example from
--   https://pandas.pydata.org/pandas-docs/version/0.23.0/generated/pandas.Series.tail.html
animals :: Frame Nat9 Int ("animal" .== Text)
animals = DF.construct $ Options.fromVec v
  where
    v :: Vec Nat9 (Rec ("animal" .== Text))
    v = (Vec.fromList . map mkRec) animalNames
          |> orCrash "Examples.animals.1"

    mkRec a = #animal .== a

    animalNames =
      [ "alligator"
      , "bee"
      , "falcon"
      , "lion"
      , "monkey"
      , "parrot"
      , "shark"
      , "whale"
      , "zebra"
      ]

animalSeries :: Series Nat9 Int Text
animalSeries = DF.asSeries animals

rendered :: Text
rendered = Text.unlines
  [ DF.render df1
  , DF.render df2
  ]

ageToText :: Rec AgeFields -> [Text]
ageToText = pure . show . (.! #age)

toTexts :: Rec PersonFields -> [Text]
toTexts rp =
  [ rp .! #name
  , show (rp .! #age)
  ]

toTexts' :: Rec NameFields -> [Text]
toTexts' rp = [ rp .! #name ]

indexesTest :: Bool
indexesTest = Just (DF.indexes df1) == Vec.fromList [0, 1]

rd :: IO ()
rd = putStr rendered

displayDf1 :: IO ()
displayDf1 = DF.display df1

flagged :: Frame Nat2 Int (PersonFields .+ "flagged" .== Bool)
flagged = DF.extend #flagged False df1

plusFull :: Rec PersonFields
         -> Rec (PersonFields .+ "fullName" .== Text)
plusFull r = Rec.extend #fullName fname r
  where
    fname = r .! #name

capitalize :: SNatI n
           => Frame n Int PersonFields
           -> Frame n Int (PersonFields .+ "capsName" .== Text)
capitalize = DF.extendWith #capsName (\r -> Text.toUpper $ r .! #name)

capitalizedDf1 :: Frame Nat2 Int (PersonFields .+ "capsName" .== Text)
capitalizedDf1 = capitalize df1

withNameLens :: SNatI n
             => Frame n Int PersonFields
             -> Frame n Int (PersonFields .+ "nameLen" .== Int)
withNameLens = DF.extendFrom #name #nameLen Text.length

nameLengthed :: Frame Nat2 Int (PersonFields .+ "nameLen" .== Int)
nameLengthed = withNameLens df1

filteredByIndexes :: Frame Nat4 Int ("animal" .== Text)
filteredByIndexes = DF.filterIndexes f animals
  where
    f =  Vec.toList
     >>> List.filter odd
     >>> Vec.fromList
     >>> fromMaybe (panic "wrong size!")

s1 :: Series Nat3 Int Float
s1 = Series.fromVec
  . fromMaybe (panic "s1.boom.1")
  . Vec.fromList
  $ [ 1.1, 2.2, 3.3 ]

s2 :: Series Nat3 Int Float
s2 = Series.fromVec
  . fromMaybe (panic "s1.boom.1")
  . Vec.fromList
  $ [ 2.1, 0.0, 8.3 ]

s1_plus1 :: Series Nat3 Int Float
s1_plus1 = Series.op (+) (1 :: Float) s1

s1_le :: Series Nat3 Int Bool
s1_le = Series.op (<=) s1 s2

s1_plusVec :: Series Nat3 Int Float
s1_plusVec = Series.op (+) v s1
  where
    v :: Vec Nat3 Float
    v = 1 ::: 20 ::: 300 ::: VNil

s1_plusSmallList :: Series Nat3 Int Float
s1_plusSmallList = Series.op (+) xs s1
  where
    xs :: NonEmpty Float
    xs = NE.fromList [1, 20]

s1_plusLargeList :: Series Nat3 Int Float
s1_plusLargeList = Series.op (+) xs s1
  where
    xs :: NonEmpty Float
    xs = NE.fromList [1, 20, 3, 30, 4, 50, 1, 2, 3]

s1_minus1 :: Series Nat3 Int Float
s1_minus1 = Series.op (-) (1 :: Float) s1

s1_div2 :: Series Nat3 Int Float
s1_div2 = Series.op (/) (2 :: Float) s1

{-# NOINLINE examples #-}
examples :: Frame Nat6 Int PersonFields
examples = unsafePerformIO $ CSV.fromHeadedCSV "data/example.csv" >>= \case
  Left error      -> panic $ "Examples.examples.1: " <> show error
  Right Nothing   -> panic   "Examples.examples.2: Nothing"
  Right (Just df) -> pure df

{-# NOINLINE examples' #-}
examples' :: Frame Nat6 Int PersonFields
examples' = unsafePerformIO
  $ CSV.unsafeFromHeadedCSV "data/example.csv"

-- product=pd.Frame({
--     'Product_ID':[101,102,103,104,105,106,107],
--     'Product_name':['Watch','Bag','Shoes','Smartphone','Books','Oil','Laptop'],
--     'Category':['Fashion','Fashion','Fashion','Electronics','Study','Grocery','Electronics'],
--     'Price':[299.0,1350.50,2999.0,14999.0,145.0,110.0,79999.0],
--     'Seller_City':['Delhi','Mumbai','Chennai','Kolkata','Delhi','Chennai','Bengalore']
-- })

type Product = "id" .== Int
            .+ "name" .== Text
            .+ "category" .== Text
            .+ "price" .== Float
            .+ "city" .== Text

data NativeProduct = NativeProduct
  { id       :: Int
  , name     :: Text
  , category :: Text
  , price    :: Float
  , city     :: Text
  } deriving (Eq, Generic, Ord, Show)

products :: SNatI n => Either Text (Frame n Int Product)
products = panic "Examples.products" -- DF.fromTexts productTexts

productTexts :: [(Text, [Text])]
productTexts =
  [ ( "id"
    , map show [101..107 :: Int]
    )
  , ( "name"
    , ["Watch","Bag","Shoes","Smartphone","Books","Oil","Laptop"]
    )
  , ( "category"
    , ["Fashion","Fashion","Fashion","Electronics","Study"
      ,"Grocery","Electronics"]
    )
  , ( "price"
    , map show [299.0,1350.50,2999.0,14999.0,145.0,110.0,79999.0 :: Float]
    )
  , ( "city"
    , ["Delhi","Mumbai","Chennai","Kolkata","Delhi","Chennai","Bengalore"]
    )
  ]

getDemoS :: IO (Series Nat6 Int Int)
getDemoS = do
  nl >> DF.display df
  nl >> Series.display s
  pure s
  where
    df = examples
    s  = df ^. DF.series #age

aDemo :: IO ()
aDemo = do
  s <- getDemoS
  nl >> adisplay (Series.filter even . map (*10) $ s)
  nl

cpsDemo :: IO ()
cpsDemo = do
  s <- getDemoS
  nl >> Series.filterThen even s (adisplay . map (*10))
  nl

cpsContDemo :: IO ()
cpsContDemo = do
  s <- getDemoS
  run $ do
    s'  <- ct $ Series.filterThen even s
    let s'' = map (*10) s'
    lift nl >> lift (a_ Series.display s'')
    lift nl
  where
    ct = ContT

aseriesDemo :: IO ()
aseriesDemo = do
  as <- Series.aseries <$> getDemoS
  let bs = afilter even as
      ds = map (*10) bs
  nl >> adisplay ds
  nl

afilter :: forall a. (a -> Bool)
        -> Series.ASeries Int a
        -> Series.ASeries Int a
afilter p = a_ (Series.filter p)

adisplay :: Series.ASeries Int Int -> IO ()
adisplay = a_ Series.display

run :: ContT r IO r -> IO r
run = flip runContT pure

monadExample :: Monad m => m Int -> m Int -> m Int
monadExample a b = do
  x <- a
  y <- b
  pure (x + y)

meSeries :: Series Nat1 Int Int
meSeries = monadExample (pure 1) (pure 2)

meList :: [Int]
meList = monadExample [1,2,3] [4,5,6]

newSeries :: Series Nat3 Int Float
newSeries = do
  x <- s1
  y <- s1
  pure $ x + y

dfPlus :: Frame Nat2 Int (PersonFields .+ "xs" .== Int)
dfPlus = DF.addSeries #xs s df1
  where
    s :: Series Nat2 Int Int
    s = Series.fromList [10, 20] `onCrash` "dfPlus.s"

someThing :: IO ()
someThing = do
  nl >> DF.display df1
  nl >> DF.display (df1 & DF.record 0 . _Just . #name %~ Text.toUpper)
  nl >> DF.display (df1 & DF.record 1 . _Just . #name %~ Text.toUpper)
  nl >> DF.display (df1 & DF.record 2 . _Just . #name %~ Text.toUpper)
  nl >> DF.display (df1 & DF.record 0 . _Just . #age *~ 15)
  nl >> DF.display (df1 & DF.record 1 . _Just . #age *~ 15)
  nl >> DF.display (df1 & DF.record 2 . _Just . #age *~ 15)
  nl

nineAddresses :: IO (Series Nat9 Int Text)
nineAddresses = Series.fake FAddress.fullAddress

type PersonX = "name"    .== Text
            .+ "phone"   .== Text
            .+ "address" .== Text

type FakeX = Map Fake PersonX

nineXs :: IO (Frame Nat9 Int PersonX)
nineXs = DF.fake
  (  #name    .== FName.name
  .+ #phone   .== FPhone.cellPhoneFormat
  .+ #address .== FAddress.fullAddress
  )

nineFunnyXs :: IO (Frame Nat9 Int PersonX)
nineFunnyXs = DF.fake
  (  #name    .== FFunny.name
  .+ #phone   .== FPhone.cellPhoneFormat
  .+ #address .== FAddress.fullAddress
  )

type ArtistFavs = "name"     .== Text
               .+ "favGod"   .== Text
               .+ "favBrand" .== Text
               .+ "strain"   .== Text

type FakeArtistFavs = Map Fake ArtistFavs

fakeArtistFavs :: forall n idx.
                  ( Enum idx
                  , SNatI n
                  , idx ~ Int  -- For now to make it easy to call
                  )
               => IO (Frame n idx ArtistFavs)
fakeArtistFavs = DF.fake
  (  #name     .== FFunny.name
  .+ #favGod   .== FAncient.god
  .+ #favBrand .== FApp.brand
  .+ #strain   .== FCannabis.strains
  )

buzzwords :: IO (Series Nat9 Int Text)
buzzwords = Series.fake FBuzz.buzzwords
