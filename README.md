# Typed Data Frames

My latest attempt to implement effective (if not efficient) DataFrames (ala
python's pandas) in Haskell.

## Notes

Fundamentally a DataFrame is a tabluar data set that has rows and columns.  The
columns are indexed by some type `idx` and the rows are labelled by some type
of label `lbl`.

Given this is Haskell we'll probably end up in a scenario where we're using
vectors for holding the rows, so we'll probably need mutable and immutable
versions of DataFrames akin to the mutable and immutable vectors.

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

In order to be able to do things like extract a single column or generate a new
column our rows need to be of a type that allows us to do that sort of thing.
One example would be a variant type like SuperRecord... but we certainly can't
be generic in this (or at least doing that would require a lot of work) so at
least for the initial POC we either want to pick an external type like
SuperRecord and make that a requirement or develop our own bespoke type.

## More

Possibly good thing to try for comparison:
- https://www.architecture-performance.fr/ap_blog/loading-data-into-a-pandas-dataframe-a-performance-study/
