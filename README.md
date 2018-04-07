[![Build Status](https://travis-ci.org/edwardwas/sized-grid.svg?branch=master)](https://travis-ci.org/edwardwas/sized-grid)

sized-grid
===========

A way of working with grids in Haskell with size encoded with at the type level.

Quick tutorial
========

This is literate Haskell file, so we start by importing our library and some other utilities.

```haskell
import SizedGrid
import Data.AffineSpace
import Data.AdditiveGroup
import Data.Distributive
import Data.Functor.Rep
```

The core datatype of this library is `Grid (cs :: '[k]) (a :: *)`. `cs` is a type level list of coordinate types. We could use a single type level number here, but by using different types we can say what happened when we move outside the bounds of a grid. There are three different coordinate types provided.

* `Ordinal n`: An ordinal can be an integral number between 0 and n-1. As numbers outside the grid are not possible, this has the most restrictive API. One can convert between an Ordinal and a number of ordinalToNum and numToOrdinal.

* `HardWrap n`: Like Oridnal, HardWrap can only hold intergral numbers between 0 and n - 1, but it allows a more permissive API by clamping values outside of its range. It is an instance of `Semigroup` and `Monoid`, where `mempty` is 0 and `<>` is addition. 

* `Periodic n`: This is the most permissive. When a value is generated outside the given range, it wraps then around using modular arithmetic. Is is an instance of `Semigroup` and `Monoid` like `HardWrap`, but also of `AdditiveGroup` allowing negation.

`HardWrap` and `Periodic` are both instances of `AffineSpace`, which their `Diff` being `Integer`. This means there are many occasion where one doesn't have to work directly with these values (which can be cumbersome) but instead work with their differences as regular numbers.

The last type value of `Grid` is the type of each element. 

The other main type is `Coord cs`, where `cs` is, again, a type level list of coordinate types. For example, `Coord '[Periodic 3, HardWrap 4]` is a coordinate in a 3 by 4 2D space. The different types (`Periodic` and `HardWrap`) tell how to handle combining theses different numbers. `Coord cs` is an instance of `Semigroup`, `Monoid` and `AdditiveGroup` if each of the coordinates is also an instance of that typeclass. `Coord` is also and instance of of `AffineSpace`, where `Diff` is a n-tuple, meaning we can pattern match and do all sorts of nice things.

There is a deliberately small number of functions that work other `Grid`: we instead opt for using typeclasses to create the required functionality. `Grid` is an instance of the following types (with some required constraints):

* `Functor`: Update all values in the grid with the same function
* `Applicative`: As the size of the grid is statically known, `pure` just creates an grid with all the same element at each point. `<*>` combines the grids point wise.
* `Monad`: I'm not sure if there is much of a need for this, but an instance exists.  
* `Foldable`: Combine each element of the grid
* `Traverse`: Apply an applicative function over the grid
* `IndexedFunctor`, `IndexedFoldable` and `IndexedTraversable`: Like `Functor`, `Foldable` and `Traversable`, but with access to the position at each point. These are from the lens package
* `Distributive`: Like `Traversable`, but the other way round. Allows us to put a functor inside the grid
* `Representable`: `Grid cs a` is isomorphic `Coord cs -> a`, so we can `tabulate` and `index` to make this conversion

We also have a `FocusedGrid` type, which is like `Grid` but has a certain focused position. This means that we loose many instances, but we game `Comonad` and `ComonadStore`. 

When dealing with areas around `Coords`, we can use `moorePoints` and `vonNeumanPoints` to generate [Moore](https://en.wikipedia.org/wiki/Moore_neighborhood) and [von Neuman](https://en.wikipedia.org/wiki/Von_Neumann_neighborhood) neighbourhoods. Note that these include the center point.

We introduce two new typeclasses: 'IsCoord' and 'IsGrid'. `IsGrid` has `gridIndex`, which allows us to get a single element of the grid and lenses to convert between `FocusedGrid` and `Grid`. `IsCoord` has `CoordSized`, which is the size of the coord and an iso to convert between `Ordinal` and the `Coord`

Example - Game of Life
=====================

```haskell
main = return ()
```


