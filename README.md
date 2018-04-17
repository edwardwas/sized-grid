[![Build Status](https://travis-ci.org/edwardwas/sized-grid.svg?branch=master)](https://travis-ci.org/edwardwas/sized-grid)

sized-grid
===========

A way of working with grids in Haskell with size encoded at the type level.

Quick tutorial
========

The core datatype of this library is `Grid (cs :: '[k]) (a :: *)`. `cs` is a type level list of coordinate types. We could use a single type level number here, but by using different types we can say what happened when we move outside the bounds of a grid. There are three different coordinate types provided.

* `Ordinal n`: An ordinal can be an integral number between 0 and n - 1. As numbers outside the grid are not possible, this has the most restrictive API. One can convert between an Ordinal and a number of ordinalToNum and numToOrdinal.

* `HardWrap n`: Like Oridnal, HardWrap can only hold intergral numbers between 0 and n - 1, but it allows a more permissive API by clamping values outside of its range. It is an instance of `Semigroup` and `Monoid`, where `mempty` is 0 and `<>` is addition. 

* `Periodic n`: This is the most permissive. When a value is generated outside the given range, it wraps that around using modular arithmetic. Is is an instance of `Semigroup` and `Monoid` like `HardWrap`, but also of `AdditiveGroup` allowing negation.

`HardWrap` and `Periodic` are both instances of `AffineSpace`, with their `Diff` being `Integer`. This means there are many occasions where one doesn't have to work directly with these values (which can be cumbersome) and can instead work with their differences as regular numbers.

The last type value of `Grid` is the type of each element. 

The other main type is `Coord cs`, where `cs` is, again, a type level list of coordinate types. For example, `Coord '[Periodic 3, HardWrap 4]` is a coordinate in a 3 by 4 2D space. The different types (`Periodic` and `HardWrap`) tell how to handle combining theses different numbers. `Coord cs` is an instance of `Semigroup`, `Monoid` and `AdditiveGroup` as long as each of the coordinates is also an instance of that typeclass. `Coord` is also an instance of of `AffineSpace`, where `Diff` is a n-tuple, meaning we can pattern match and do all sorts of nice things.

There is a deliberately small number of functions that work over `Grid`: we instead opt for using typeclasses to create the required functionality. `Grid` is an instance of the following types (with some required constraints):

* `Functor`: Update all values in the grid with the same function
* `Applicative`: As the size of the grid is statically known, `pure` just creates a grid with the same element at each point. `<*>` combines the grids point wise.
* `Monad`: I'm not sure if there is much of a need for this, but an instance exists.  
* `Foldable`: Combine each element of the grid
* `Traverse`: Apply an applicative function over the grid
* `IndexedFunctor`, `IndexedFoldable` and `IndexedTraversable`: Like `Functor`, `Foldable` and `Traversable`, but with access to the position at each point. These are from the lens package
* `Distributive`: Like `Traversable`, but the other way round. Allows us to put a functor inside the grid
* `Representable`: `Grid cs a` is isomorphic `Coord cs -> a`, so we can `tabulate` and `index` to make this conversion

We also have a `FocusedGrid` type, which is like `Grid` but has a certain focused position. This means that we lose many instances, but we gain `Comonad` and `ComonadStore`. 

When dealing with areas around `Coord`s, we can use `moorePoints` and `vonNeumanPoints` to generate [Moore](https://en.wikipedia.org/wiki/Moore_neighborhood) and [von Neuman](https://en.wikipedia.org/wiki/Von_Neumann_neighborhood) neighbourhoods. Note that these include the center point.

We introduce two new typeclasses: `IsCoord` and `IsGrid`. `IsGrid` has `gridIndex`, which allows us to get a single element of the grid and lenses to convert between `FocusedGrid` and `Grid`. `IsCoord` has `CoordSized`, which is the size of the coord and an iso to convert between `Ordinal` and the `Coord`.

Example - Game of Life
=====================

As is traditional for anything with grids and comonads in Haskell, we can reimplement [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life).

This is a literate Haskell file, so we start by turning on some language extensions, importing our library and some other utilities.

```haskell
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}

import SizedGrid

import Control.Comonad
import Control.Lens
import Control.Comonad.Store
import Data.AdditiveGroup
import Data.AffineSpace
import Data.Distributive
import Data.Functor.Rep
import Data.Semigroup (Semigroup(..))
import GHC.TypeLits
import System.Console.ANSI
```

We create a datatype for alive or dead.

```haskell
data TileState = Alive | Dead deriving (Eq,Show)
```

We encode the rules of the game via a step function.

```haskell
type Rule = TileState -> [TileState] -> TileState

gameOfLife :: Rule
gameOfLife here neigh =
    let aliveNeigh = length $ filter (== Alive) neigh
    in if | here == Alive && aliveNeigh `elem` [2,3] -> Alive
          | here == Dead && aliveNeigh == 3 -> Alive
          | otherwise -> Dead
```

We can then write a function to apply this to every point in a grid.

```haskell
applyRule :: 
       ( All IsCoord cs
       , All Monoid cs
       , All Semigroup cs
       , All AffineSpace cs
       , All Eq cs
       , AllDiffSame Integer cs
       , KnownNat (MaxCoordSize cs)
       , IsGrid cs (grid cs)
       )
    => Rule
    -> grid cs TileState
    -> grid cs TileState
applyRule rule = over asFocusedGrid $ 
    extend $ \fg -> rule (extract fg) $ map (\p -> peek p fg) $ 
        filter (/= pos fg) $ moorePoints (1 :: Integer) $ pos fg

```

We can create a simple drawing function to display it to the screen.

```haskell
displayTileState :: TileState -> Char
displayTileState Alive = '#'
displayTileState Dead = '.'

displayGrid :: (KnownNat (CoordSized x), KnownNat (CoordSized y)) => 
      Grid '[x, y] TileState -> String
displayGrid = unlines . collapseGrid . fmap displayTileState
```

Let's create a glider, and watch it move!

```haskell
glider :: 
      ( KnownNat (CoordSized x * CoordSized y)
      , Semigroup x
      , Semigroup y
      , Monoid x
      , Monoid y
      , IsCoord x
      , IsCoord y
      , AffineSpace x
      , AffineSpace y
      , Diff x ~ Integer
      , Diff y ~ Integer
      ) 
      => Coord '[x,y] 
      -> Grid '[x,y] TileState
glider offset = pure Dead 
    & gridIndex (offset .+^ (0,-1)) .~ Alive
    & gridIndex (offset .+^ (1,0)) .~ Alive
    & gridIndex (offset .+^ (-1,1)) .~ Alive
    & gridIndex (offset .+^ (0,1)) .~ Alive
    & gridIndex (offset .+^ (1,1)) .~ Alive
```

We can now make our glider run!

```haskell
run = 
    let start :: Grid '[Periodic 10, Periodic 10] TileState 
        start = glider (mempty .+^ (3,3))
        doStep grid = do
          clearScreen
          putStrLn $ displayGrid grid
          _ <- getLine
          doStep $ applyRule gameOfLife grid
    in doStep start

main = return ()
```
