sized-grid
===========

A way of working with grids in Haskell with size encoded with at the type level.

Quick Tutorial
-------------

Coord
--------

Altough the size of the grid is known at compile time, it is useful to describe what happens when we want to move outside of the grid. There are three different possiblityies included in this library.

* Periodic - A position outside of the grid wraps around back onto the grid
* HardWrap - Positions outside of the grid are clamped to be inside the grid (Still TODO)
* Ordinal - It is impossible to create positions outside the grid, but many functions produce results wrapped in `Maybe`, and we loose many useful instances.

Grid
-----

The core data type is `Grid`, which has the following form.

```
Grid (cs :: '[k]) a
```

where `cs` is a type level list of Coordinates, and `a` is the type of each element in the grid. There are deliberately a small number of bespoke functions which operate on this: instead we choose to use some commonly known classes. `Grid` is an instance or `Functor`, `Foldable` and  `Traversable`, whose behaviour should be known to Haskell users. It is also an instance of `Applicative`, which pure creating a grid with identical `a`s at each site, and `ap` overlays two grids.
