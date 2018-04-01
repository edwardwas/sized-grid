sized-grid
===========

A way of working with grids in Haskell with size encoded with at the type level.

Quick Tutorial
-------------

The core data type is `Grid`, which has the following form.

```
Grid (cs :: '[k]) a
```

where `cs` is a type level list of Coordinates, and `a` is the type of each element in the grid. There are deliberately a small number of bespoke functions which operate on this: instead we choose to use some commonly known classes. `Grid` is an instance or `Functor`, `Foldable` and  `Traversable`, whose behaviour should be known to Haskell users. It is also an instance of `Applicative`, which pure creating a grid with identical 'a's at each site, and `ap` overlays two grids.
