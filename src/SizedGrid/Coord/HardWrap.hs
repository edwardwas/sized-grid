{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module SizedGrid.Coord.HardWrap where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal
import           SizedGrid.Peano
import           SizedGrid.Type.Number

import           Control.Lens          (iso)
import           Data.AffineSpace
import           Data.Maybe            (fromJust)
import           Data.Proxy            (Proxy (..))
import           Data.Semigroup        (Semigroup (..))
import           System.Random         (Random (..))

newtype HardWrap (n :: Peano) = HardWrap
    { unHardWrap :: Ordinal n
    } deriving (Eq,Show,Ord)

deriving instance (n ~ (S x), SPeanoI x) => Random (HardWrap n)
deriving instance (n ~ (S x), SPeanoI x) => Enum (HardWrap n)
deriving instance (n ~ (S x), SPeanoI x) => Bounded (HardWrap n)

instance (SPeanoI n) => IsCoord (HardWrap n) where
    type CoordSized (HardWrap n) = n
    asOrdinal = iso unHardWrap HardWrap
    sCoordSized _ = sPeano
    maxCoordSize p = demoteSPeano (sCoordSized p) - 1

instance (n ~ S x, SPeanoI x) => Semigroup (HardWrap n) where
    HardWrap a <> HardWrap b =
        HardWrap $
        fromJust $
        numToOrdinal $
        min
            (maxCoordSize (Proxy @(HardWrap n)))
            (ordinalToNum a + ordinalToNum b)

instance (n ~ S x, SPeanoI x) => Monoid (HardWrap n) where
  mempty = HardWrap OZ
  mappend = (<>)

instance (n ~ S x, SPeanoI x) => AffineSpace (HardWrap n) where
    type Diff (HardWrap n) = Integer
    HardWrap a .-. HardWrap b =
        max 0 $
        min
            (fromIntegral $ maxCoordSize (Proxy @(HardWrap n)))
            (ordinalToNum a - ordinalToNum b)
    HardWrap a .+^ b = HardWrap $ fromJust $ numToOrdinal $
        max 0 $
        min (maxCoordSize (Proxy @(HardWrap n))) $
        ((ordinalToNum a) + fromIntegral b)
