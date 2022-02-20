# Data.Grid

We're going try to be a little more principled about things this time, so to
start with, let's write down what we're going to try to build, and maybe some
of the why as well.

First we'll have `Series` which are wrappers around two fixed-sized vectors -
an index vector and a value vector and which will approximate the `Series` type
from pandas.  Then we will have `Frame`s which are a `Series` of `Series`, and
approximate `DataFrame`s from pandas.

For `Index`es we will start with:
- `Categorical`
- `Multi`
- `Range`
- `Vector`

And see how far we can get.  We will NOT enforce an Enum constraint on indexes
this time.

We will attempt the mutable version alongside the immutable version this time.

We will avoid all the crashing and propagate maybes as appropriate.

We will use lenses in a principled way this time -- liberally internally but
only exporting optics that don't violate safety.

We will take a very Haskell (or at least John's version of Haskell) flavored
approach from the get go this time.

With (very) few exceptions we will try to spend our time on getting the types
right and letting the rest follow.  Most implementations of most functions
should be one liners.

We are going to try to be very TDDish about things but we'll see how that goes.

We will reserve `map` for Protolude's `fmap` and give specific names to any
map-like functions we create.

We will intentionally be as minimal as possible in the core and put any fancy
stuff into add-on packages.
