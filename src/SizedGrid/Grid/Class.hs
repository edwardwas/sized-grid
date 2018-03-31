{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}

module SizedGrid.Grid.Class where

import           SizedGrid.Coord
import           SizedGrid.Coord.Class
import           SizedGrid.Grid.Focused
import           SizedGrid.Grid.Grid

import           Control.Lens           hiding (index)
import           Data.Functor.Rep
import           Data.Semigroup         hiding (All (..))
import           Generics.SOP

class IsGrid cs grid | grid -> cs where
  gridIndex :: Coord cs -> Lens' (grid a) a
  asGrid :: Lens' (grid a) (Grid cs a)
  asFocusedGrid :: Lens' (grid a) (FocusedGrid cs a)

instance (All Semigroup cs, All Monoid cs, All IsCoord cs) =>
         IsGrid cs (Grid cs) where
    gridIndex coord =
        lens
            (\g -> index g coord)
            (\(Grid v) a -> Grid (v & ix (coordPosition coord) .~ a))
    asGrid = id
    asFocusedGrid = lens (\g -> FocusedGrid g mempty) (\g fg -> focusedGrid fg)

instance (All IsCoord cs, All Monoid cs, All Semigroup cs) =>
         IsGrid cs (FocusedGrid cs) where
    gridIndex c =
        (\f (FocusedGrid g p) -> (\g' -> FocusedGrid g' p) <$> f g) .
        gridIndex c
    asGrid = lens focusedGrid (\(FocusedGrid _ p) g -> FocusedGrid g p)
    asFocusedGrid = id
