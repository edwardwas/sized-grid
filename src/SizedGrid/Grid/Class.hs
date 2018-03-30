{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}

module SizedGrid.Grid.Class where

import           SizedGrid.Coord

import           Control.Lens

class IsGrid cs grid | grid -> cs where
  gridIndex :: Coord cs -> Lens' (grid a) a
