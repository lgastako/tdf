# Typed Data Frames

My latest attempt to implement effective (if not efficient) DataFrames (ala
python's pandas) in Haskell.

## Notes

Fundamentally a DataFrame is a tabluar data set that has rows and columns.  The
columns are indexed by some type `idx` and the rows are indexed by row types
from the `row-types` package.

We take vectors of rows as input and turn it into rows of vectors to operate on.

** BIG TODO: mutability **

- TODO List:
  - Add usecols and nrows functionality to CSV reader
  - Tests to ensure indexes are properly preserved along the lines of
    whatever Pandas does
  - Add rows
  - Remove rows
  - Perform aggregate computations over the rows
  - TODOs in code... in particular:
    - eliminating the Forall on column counts and the like

## More

Possibly good thing to try for comparison:
- https://www.architecture-performance.fr/ap_blog/loading-data-into-a-pandas-dataframe-a-performance-study/
