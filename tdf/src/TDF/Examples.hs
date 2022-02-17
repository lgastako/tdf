{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module TDF.Examples where

import           TDF.Prelude            hiding ( Product
                                               , drop
                                               , size
                                               , take
                                               )

import           Control.Monad.Cont            ( ContT( ContT )
                                               , runContT
                                               )
import qualified Data.Row.Records   as Rec
import qualified Data.List          as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as Text
import qualified Data.Vec.Lazy      as Vec
import           TDF.DataFrame                 ( Axes
                                               , DataFrame
                                               )
import qualified TDF.CSV            as CSV
import qualified TDF.DataFrame      as DF
import qualified TDF.Options        as Options
import           TDF.Series                    ( Series
                                               , a_
                                               )
import qualified TDF.Series         as Series
import           System.IO.Unsafe              ( unsafePerformIO )

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

person2Times100 :: Person
person2Times100 = Rec.update #age 4500 person2

greet :: (r ≈ "name" .== Text)
      => Rec r
      -> Text
greet = ("Hello " <>) . (.! #name)

df1 :: DataFrame Nat2 Int PersonFields
df1 = DF.fromList
  [ person
  , person2
  ]
  & fromMaybe (panic "Examples.df1")

df1Times100 :: DataFrame Nat2 Int PersonFields
df1Times100 = DF.fromList
  [ personTimes100
  , person2Times100
  ]
  & fromMaybe (panic "Examples.df1Times100")

df1Renamed :: DataFrame Nat2 Int FullPersonFields
df1Renamed = DF.rename #name #fullName df1

-- TODO can we make it easy to do the same thing with just "age" (or #age) to
--      avoid having to know the type... where the return type is whatever the
--      type of "age" is in in df1?  maybe we don't want to lose the type info?
--      so perhaps DF.restrict replaces both select_types and whatever other
--      subsetting facilities? let's assume so for now.
ages :: DataFrame Nat2 Int ("age" .== Int)
ages = DF.restrict df1

df1Axes :: Axes Int
df1Axes = DF.axes df1

-- Neat.
df1Restricted :: DataFrame Nat2 Int NameFields
df1Restricted = DF.restrict df1

-- λ> DF.display $ DF.restrict @NameFields df1
-- name
-- ----
-- Alex
-- Dave

df1' :: DataFrame Nat2 Int PersonFields
df1' = df1 -- undefined -- DF.reindex [0, 1] df1

-- λ> DF.memSize df1'
-- 1096
-- Yeesh that's a lot of overhead for [{name:Alex,age:23},{name:Dave,age:45}]

df1'' :: DataFrame Nat2 Int (PersonFields .+ "foo" .== Text)
df1'' = DF.map (\x -> x .+ #foo .== ("bar" ::Text)) df1'

df2 :: DataFrame Nat2 Int NameFields
df2 = DF.map justName df1

df3' :: DataFrame Nat2 Int (PersonFields .+ "fullName" .== Text)
df3' = DF.map plusFull df1

df3 :: DataFrame Nat2 Int NameFields
df3 = DF.column #name df1

df6 :: DataFrame Nat3 Int PersonFields
df6 = DF.fromNativeVec nativeVector

nativeVector :: Vec Nat3 Person'
nativeVector = Vec.fromList
  [ Person' "bob"  86
  , Person' "dave" 55
  , Person' "john" 46
  ]
  & fromMaybe (panic "nativeVector")

something :: VecPerson Nat3
something = Rec.distribute . DF.toVec $ df6

person's :: [Person']
person's = Vec.toList . DF.toNativeVec $ df6

type DateExampleFields = "year"  .== Integer
                      .+ "month" .== Int
                      .+ "day"   .== Int
                      .+ "value" .== Int


type DateColsLen = Nat6

dateAcrossCols :: DataFrame DateColsLen Int DateExampleFields
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
-- -- I would like to avoid having the core DataFrame package avoid a dependency
-- -- on the `time` package.  So I'll probably move this out to separate add-on
-- -- lib sooner or later.
-- dateInCol :: DataFrame DateColsLen Int ("date" .== Day .+ "value" .== Int)
-- dateInCol = dateAcrossCols
--   & DF.extendWithDay #date
--       ((,,) <$> (.! #year) <*> (.! #month) <*> (.! #day))
--   & DF.restrict

-- Example from
--   https://pandas.pydata.org/pandas-docs/version/0.23.0/generated/pandas.Series.tail.html
animals :: DataFrame Nat9 Int ("animal" .== Text)
animals = DF.construct $ Options.fromVec v
  where
    v :: Vec Nat9 (Rec ("animal" .== Text))
    v = (Vec.fromList . map mkRec $ animalNames)
          & fromMaybe (panic "Examples.animals.1")

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

flagged :: DataFrame Nat2 Int (PersonFields .+ "flagged" .== Bool)
flagged = DF.extend #flagged False df1

plusFull :: Rec PersonFields
         -> Rec (PersonFields .+ "fullName" .== Text)
plusFull r = Rec.extend #fullName fname r
  where
    fname = r .! #name

capitalize :: SNatI n
           => DataFrame n Int PersonFields
           -> DataFrame n Int (PersonFields .+ "capsName" .== Text)
capitalize = DF.extendWith #capsName (\r -> Text.toUpper $ r .! #name)

capitalizedDf1 :: DataFrame Nat2 Int (PersonFields .+ "capsName" .== Text)
capitalizedDf1 = capitalize df1

withNameLens :: SNatI n
             => DataFrame n Int PersonFields
             -> DataFrame n Int (PersonFields .+ "nameLen" .== Int)
withNameLens = DF.extendFrom #name #nameLen Text.length

nameLengthed :: DataFrame Nat2 Int (PersonFields .+ "nameLen" .== Int)
nameLengthed = withNameLens df1

filteredByIndexes :: DataFrame Nat4 Int ("animal" .== Text)
filteredByIndexes = DF.filterIndexes f animals
  where
    f =  Vec.toList
     >>> List.filter odd
     >>> Vec.fromList
     >>> fromMaybe (panic "wrong size!")

s1 :: Series Nat3 Int Float
s1 = fromMaybe (panic "s1.boom.2")
  . Series.fromVec
  . fromMaybe (panic "s1.boom.1")
  . Vec.fromList
  $ [ 1.1, 2.2, 3.3 ]

s2 :: Series Nat3 Int Float
s2 = fromMaybe (panic "s1.boom.2")
  . Series.fromVec
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
examples :: DataFrame Nat6 Int PersonFields
examples = unsafePerformIO $ CSV.fromHeadedCSV "data/example.csv" >>= \case
  Left error      -> panic $ "Examples.examples.1: " <> show error
  Right Nothing   -> panic   "Examples.examples.2: Nothing"
  Right (Just df) -> pure df

{-# NOINLINE examples' #-}
examples' :: DataFrame Nat6 Int PersonFields
examples' = unsafePerformIO
  $ CSV.unsafeFromHeadedCSV "data/example.csv"

-- product=pd.DataFrame({
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

products :: SNatI n => Either Text (DataFrame n Int Product)
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

aDemo :: IO ()
aDemo = do
  let df = examples
      s  = df ^. DF.series #age
  nl >> DF.display df
  nl >> Series.display s
  nl >> Series.filterThen even s (a_ Series.display)
  nl

aContDemo :: IO ()
aContDemo = do
  let df = examples
      s  = df ^. DF.series #age
  nl >> DF.display df
  nl >> Series.display s
  run $ do
    s' <- ContT $ Series.filterThen even s
    lift nl >> lift (a_ Series.display s')
    lift nl

run :: ContT r IO r -> IO r
run = flip runContT pure
