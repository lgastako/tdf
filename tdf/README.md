# Typed Data Frames

My latest attempt to implement effective (if not efficient) DataFrames (ala
python's pandas) in Haskell.

Candidate names:
- chassis

## Biggest Problems to Solve

NB. `append` is deprecated in pandas and replaced by `concat.

* Column ordering (Series of Series?)
* Handle non-Label friendly column names well
  (and then un-hack all the example csv files)
* Dynamic rendering failures
  - Either fix the rendering falures; or, ideally...
  - Eliminate the need for the whole Dynamic dance
* Proper indexes
* Finish all the a_ implementations
  - Including moving the A* versions out of the current modules into
    their own sub-modules
* Melt, append, concat, other dynamic shape changers.
  - Maybe existentials will save us here
* Mutable implementation
  - Possibly using the same existential wrapper type of deal as
    the ASeries, AFrame, etc.
* Lots more tests

## Example CSVs

You can see some examples of manipulating CSV files with data frames on the
following pages:

https://pypancsv.github.io/pypancsv/quickexamples/#first-the-csv-files-within-the-examples
https://www.geeksforgeeks.org/python-pandas-dataframe/
https://www.analyticsvidhya.com/blog/2020/02/joins-in-pandas-master-the-different-types-of-joins-in-python/
https://realpython.com/python-data-cleaning-numpy-pandas/

You can find some of the CSV files from those sites in the `test/fixtures`
directory.

## Notes

Fundamentally a DataFrame is a tabluar data set that has rows and columns.  The
columns are indexed by some type `idx` and the rows are indexed by row types
from the `row-types` package.

We take vectors of rows as input and turn it into rows of vectors to operate on.

** BIG TODO: mutability **

- Bugs
  - asSeries (eg animalSeries in Examples.hs) ends up show'ing strings
    on the ToField/FromField round trip I'm guessing?  But it shouldn't
    even be doing a round trip there...

- TODO List:
  - IN-PROGRESS lenses for bidirectional accessors like loc, at, iat, etc.
  - pd.describe
  - merge
    https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.merge.html
    https://pandas-docs.github.io/pandas-docs-travis/user_guide/merging.html
    https://pandas.pydata.org/pandas-docs/stable/user_guide/merging.html
    https://pypancsv.github.io/pypancsv/quickexamples/#first-the-csv-files-within-the-examples
  - append/join/etc too
    https://datagy.io/pandas-merge-concat/
    https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.join.html#pandas.DataFrame.join
    https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.append.html
  - reseting of indexes
    https://pythonexamples.org/pandas-dataframe-reset-index/
  - Add usecols and nrows functionality to CSV reader
  - Tests to ensure indexes are properly preserved along the lines of
    whatever Pandas does
  - Insert rows at a specific position in the middle
  - Remove rows from specific positions
  - Perform aggregate computations over the rows
  - Full range of pandas indexing options including hierarchical indexes.
    https://pandas-docs.github.io/pandas-docs-travis/user_guide/advanced.html#advanced-hierarchical
  - TODOs in code...
  - Clean up/standardize on terminology.
  - https://stackoverflow.com/questions/29954263/what-does-the-term-broadcasting-mean-in-pandas-documentation

## More

- https://stackoverflow.com/questions/53217607/how-do-i-operate-on-a-dataframe-with-a-series-for-every-column
- https://www.youtube.com/watch?v=iYie42M1ZyU
- https://www.kdnuggets.com/2021/03/pandas-big-data-better-options.html

High perf vectors?
- https://hackage.haskell.org/package/accelerate-1.3.0.0/docs/Data-Array-Accelerate.html
- https://hackage.haskell.org/package/repa

Possibly good thing to try for comparison:
- https://www.architecture-performance.fr/ap_blog/loading-data-into-a-pandas-dataframe-a-performance-study/

## To consider:

    {-# LANGUAGE DuplicateRecordFields     #-}
    {-# LANGUAGE NoMonomorphismRestriction #-}

## Similar Work:

- https://hackage.haskell.org/package/Frames
  Uses `vinyl` records instead of `row-types` and regular `Vector`s instead
  of fixed-sized `vecs` but otherwise pretty similar.

  Notably provides a streaming interface (via pipes) which I should investigate.

