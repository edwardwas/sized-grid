{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module SizedGrid.Grid.Focused where

import           SizedGrid.Coord
import           SizedGrid.Coord.Class
import           SizedGrid.Grid.Grid

import           Control.Comonad
import           Control.Comonad.Store
import           Data.Functor.Rep
import           Data.Semigroup        (Semigroup (..))
import           Generics.SOP

-- | Similar to `Grid`, but this has a focus on a certain square. Becuase of this we loose some instances, such as `Applicative`, but we gain a `Comonad` and `ComonadStore` instance. We can convert between a focused and unfocused list using facilites in `IsGrid`
data FocusedGrid cs a = FocusedGrid
    { focusedGrid         :: Grid cs a
    , focusedGridPosition :: Coord cs
    } deriving (Functor,Foldable,Traversable)

instance ( AllSizedKnown cs
         , All IsCoordLifted cs
         , All Monoid cs
         , All Semigroup cs
         , SListI cs
         ) =>
         Comonad (FocusedGrid cs) where
    extract (FocusedGrid g p) = index g p
    duplicate (FocusedGrid g p) = FocusedGrid (tabulate (FocusedGrid g)) p

instance ( AllSizedKnown cs
         , All IsCoordLifted cs
         , All Monoid cs
         , All Semigroup cs
         , SListI cs
         ) =>
         ComonadStore (Coord cs) (FocusedGrid cs) where
    pos = focusedGridPosition
    peek p (FocusedGrid g _) = index g p
    peeks func (FocusedGrid g p) = index g (func p)
    seek p (FocusedGrid g _) = FocusedGrid g p
    seeks func (FocusedGrid g p) = FocusedGrid g $ func p
