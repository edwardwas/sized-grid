{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module SizedGrid.Coord.Periodic where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal

import           Control.Lens
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Maybe            (fromJust)
import           Data.Proxy
import           Data.Semigroup
import           GHC.TypeLits
import           System.Random

newtype Periodic (n :: Nat) = Periodic
    { unPeriodic :: Ordinal n
    } deriving (Eq, Show, Ord)

deriving instance (1 <= n, KnownNat n) => Random (Periodic n)

instance (1 <= n, KnownNat n) => Enum (Periodic n) where
    toEnum x =
        Periodic $
        fromJust $
        numToOrdinal $
        (fromIntegral x) `mod` (maxCoordSize (Proxy @(Periodic n)))
    fromEnum (Periodic o) = ordinalToNum o

instance (1 <= n, KnownNat n) => IsCoord (Periodic n) where
    type CoordSized (Periodic n) = n
    asOrdinal = iso unPeriodic Periodic

instance (1 <= n, KnownNat n) => Semigroup (Periodic n) where
    Periodic a <> Periodic b =
        let n = maxCoordSize (Proxy :: Proxy (Periodic n)) + 1
        in Periodic $
           fromJust $ numToOrdinal ((ordinalToNum a + ordinalToNum b) `mod` n)

instance (1 <= n, KnownNat n) => Monoid (Periodic n) where
    mappend = (<>)
    mempty = Periodic minBound

instance (1 <= n, KnownNat n) => AdditiveGroup (Periodic n) where
    zeroV = mempty
    (^+^) = (<>)
    negateV (Periodic o) =
        let n = maxCoordSize (Proxy @(Periodic n)) + 1
        in Periodic $ fromJust $ numToOrdinal (negate (ordinalToNum o) `mod` n)

instance (1 <= n, KnownNat n) => AffineSpace (Periodic n) where
    type Diff (Periodic n) = Integer
    Periodic a .-. Periodic b =
        (ordinalToNum a - ordinalToNum b) `mod`
        (fromIntegral $ maxCoordSize (Proxy @(Periodic n)) + 1)
    Periodic a .+^ b =
        Periodic $
        fromJust $
        numToOrdinal $
        (ordinalToNum a + b) `mod`
        (fromIntegral $ maxCoordSize (Proxy @(Periodic n)) + 1)
