{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module SizedGrid.Coord.HardWrap where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal

import           Control.Lens          (iso)
import           Data.Aeson
import           Data.AffineSpace
import           Data.Maybe            (fromJust)
import           Data.Proxy            (Proxy (..))
import           Data.Semigroup        (Semigroup (..))
import           GHC.TypeLits
import           System.Random         (Random (..))

-- | A coordinate that clamps its numbers
newtype HardWrap (n :: Nat) = HardWrap
    { unHardWrap :: Ordinal n
    } deriving (Eq,Show,Ord)

deriving instance (KnownNat n, 1 <= n) => Random (HardWrap n)
deriving instance (KnownNat n, 1 <= n) => Enum (HardWrap n)
deriving instance (KnownNat n, 1 <= n) => Bounded (HardWrap n)
deriving instance KnownNat n => ToJSON (HardWrap n)
deriving instance KnownNat n => FromJSON (HardWrap n)
deriving instance KnownNat n => ToJSONKey (HardWrap n)
deriving instance KnownNat n => FromJSONKey (HardWrap n)

instance (1 <= n, KnownNat n) => IsCoord (HardWrap n) where
    type CoordSized (HardWrap n) = n
    asOrdinal = iso unHardWrap HardWrap

instance (1 <= n, KnownNat n) => Semigroup (HardWrap n) where
    HardWrap a <> HardWrap b =
        HardWrap $
        fromJust $
        numToOrdinal $
        min
            (maxCoordSize (Proxy @(HardWrap n)))
            (ordinalToNum a + ordinalToNum b)

instance (KnownNat n, 1 <= n) => Monoid (HardWrap n) where
  mempty = HardWrap minBound
  mappend = (<>)

instance (1 <= n, KnownNat n) => AffineSpace (HardWrap n) where
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
