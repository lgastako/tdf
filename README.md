# Typed Data Frames

My latest attempt to implement effective (if not efficient) DataFrames (ala
python's pandas) in Haskell.

## Notes

Fundamentally a DataFrame is a tabluar data set that has rows and columns.  The
columns are indexed by some type `idx` and the rows are indexed by row types
from the `row-types` package.

We take vectors of rows as input and turn it into rows of vectors to operate on.

TODO: mutability

DataFrames enable the following types of operations:

- Add columns
- Remove columns
- Add rows
- Remove rows
- Perform aggregate computations over the rows
- Calculate new columns as a function of existing rows
- Create from a list of Haskell data types
- Generate from a generator function like vectors

In general we want most operations that change the DataFrame to return another
DataFrame so that labels and indexes are maintained.  Of course we have
operations for extract lists, scalars, etc. from the DataFrames for further
processing.

## More

Possibly good thing to try for comparison:
- https://www.architecture-performance.fr/ap_blog/loading-data-into-a-pandas-dataframe-a-performance-study/
