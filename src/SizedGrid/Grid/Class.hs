{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UndecidableInstances   #-}

module SizedGrid.Grid.Class where

import           SizedGrid.Coord
import           SizedGrid.Coord.Class
import           SizedGrid.Grid.Focused
import           SizedGrid.Grid.Grid

import           Control.Lens           hiding (index)
import           Data.Functor.Rep
#if MIN_VERSION_base(4,11,0)
#else
import           Data.Semigroup         hiding (All (..))
#endif

import           Generics.SOP
import qualified GHC.TypeLits           as GHC

-- | Conversion between `Grid` and `FocusedGrid` and access grids at a `Coord`
class IsGrid cs grid | grid -> cs where
  -- | Get the element at a grid location. This is a lens because we know it must exist
  gridIndex :: Coord cs -> Lens' (grid a) a
  -- | Convert to, or run a function over, a `Grid`
  asGrid :: Lens' (grid a) (Grid cs a)
  -- | Convert to, or run a function over, a `FocusedGrid`
  asFocusedGrid :: Lens' (grid a) (FocusedGrid cs a)

instance (GHC.KnownNat (MaxCoordSize cs), All IsCoord cs) =>
         IsGrid cs (Grid cs) where
    gridIndex coord =
        lens
            (\g -> index g coord)
            (\(Grid v) a -> Grid (v & ix (coordPosition coord) .~ a))
    asGrid = id
    asFocusedGrid =
        lens (\g -> FocusedGrid g zeroCoord) (\_ fg -> focusedGrid fg)

instance (GHC.KnownNat (MaxCoordSize cs), All IsCoord cs) =>
         IsGrid cs (FocusedGrid cs) where
    gridIndex c =
        (\f (FocusedGrid g p) -> (\g' -> FocusedGrid g' p) <$> f g) .
        gridIndex c
    asGrid = lens focusedGrid (\(FocusedGrid _ p) g -> FocusedGrid g p)
    asFocusedGrid = id
