{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module SizedGrid.Grid.Grid where

import           SizedGrid.Coord
import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal
import           SizedGrid.Peano
import           SizedGrid.Type.Number

import           SizedGrid.Coord.Periodic

import           Control.Lens             hiding (index)
import           Control.Monad.State
import           Data.AffineSpace
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Proxy               (Proxy (..))
import           Data.Semigroup           (Semigroup (..))
import qualified Data.Vector              as V
import           Generics.SOP
import qualified GHC.TypeLits             as GHC
import           System.Random


data Grid (cs :: [*]) a = Grid
    { unGrid :: V.Vector a
    } deriving (Eq, Show, Functor, Foldable, Traversable)

instance GHC.KnownNat (AsNat (MaxCoordSize cs)) => Applicative (Grid cs) where
    pure =
        Grid .
        V.replicate
            (fromIntegral $ GHC.natVal (Proxy :: Proxy (AsNat (MaxCoordSize cs))))
    Grid fs <*> Grid as = Grid $ V.zipWith ($) fs as

instance (GHC.KnownNat (MaxCoordSize cs), All IsCoord cs, All Monoid cs, All Semigroup cs) =>
         Distributive (Grid cs) where
    distribute = distributeRep

instance (All IsCoord cs, All Monoid cs, All Semigroup cs, GHC.KnownNat (MaxCoordSize cs)) =>
         Representable (Grid cs) where
    type Rep (Grid cs) = Coord cs
    tabulate func = imap (\c _ -> func c) $ pure ()
    index (Grid v) c = v V.! coordPosition c

instance (All IsCoord cs, All Monoid cs, All Semigroup cs) =>
         FunctorWithIndex (Coord cs) (Grid cs) where
    imap func (Grid v) = Grid $ V.zipWith func (V.fromList allCoord) v

instance (All IsCoord cs, All Monoid cs, All Semigroup cs) =>
         FoldableWithIndex (Coord cs) (Grid cs) where
    ifoldMap func (Grid v) = foldMap id $ V.zipWith func (V.fromList allCoord) v

instance (All IsCoord cs, All Monoid cs, All Semigroup cs) =>
         TraversableWithIndex (Coord cs) (Grid cs) where
    itraverse func (Grid v) =
        Grid <$> sequenceA (V.zipWith func (V.fromList allCoord) v)

type family Tail xs where
  Tail (x ': xs) = xs

type family UnsafeGrid cs a where
  UnsafeGrid '[] a = a
  UnsafeGrid (c ': cs) a = [UnsafeGrid cs a]

gridToLists :: forall cs a . SListI cs => Grid cs a -> UnsafeGrid cs a
gridToLists (Grid v) =
    let helper = undefined
    in case (shape :: Shape cs) of
           ShapeNil -> head $ V.toList v
