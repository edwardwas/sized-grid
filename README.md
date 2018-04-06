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
```

The core datatype of this library is `Grid (cs :: '[k]) (a :: *)`. `cs` is a type level list of coordinate types. We could use a single type level number here, but by using different types we can say what happened when we move outside the bounds of a grid. There are three different coordinate types provided.

* `Ordinal n`: An ordinal can be an integral number between 0 and n-1. As numbers outside the grid are not possible, this has the most restrictive API. One can convert between an Ordinal and a number of ordinalToNum and numToOrdinal.

* `HardWrap n`: Like Oridnal, HardWrap can only hold intergral numbers between 0 and n - 1, but it allows a more permissive API by clamping values outside of its range. It is an instance of `Semigroup` and `Monoid`, where `mempty` is 0 and `<>` is addition. 

* `Periodic n`: This is the most permissive. When a value is generated outside the given range, it wraps then around using modular arithmetic. Is is an instance of `Semigroup` and `Monoid` like `HardWrap`, but also of `AdditiveGroup` allowing negation.

`HardWrap` and `Periodic` are both instances of `AffineSpace`, which their `Diff` being `Integer`. This means there are many occasion where one doesn't have to work directly with these values (which can be cumbersome) but instead work with their differences as regular numbers.

The last type value of `Grid` is the type of each element. 

The other main type is `Coord cs`, where `cs` is, again, a type level list of coordinate types. For example, `Coord '[Periodic 3, HardWrap 4]` is a coordinate in a 3 by 4 2D space. The different types (`Periodic` and `HardWrap`) tell how to handle combining theses different numbers. `Coord cs` is an instance of `Semigroup`, `Monoid` and `AdditiveGroup` if each of the coordinates is also an instance of that typeclass. 

```haskell
main = return ()
```


