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
{-# LANGUAGE UndecidableInstances       #-}

module SizedGrid.Coord.Periodic where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal
import           SizedGrid.Peano
import           SizedGrid.Type.Number

import           Control.Lens
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Maybe            (fromJust)
import           Data.Proxy
import           Data.Semigroup
import           GHC.TypeLits
import           System.Random

newtype Periodic (n :: k) = Periodic
    { unPeriodic :: Ordinal (AsPeano n)
    } deriving (Eq,Show,Ord)

deriving instance (AsPeano n ~ (S x), SPeanoI x) => Random (Periodic n)

instance (IsTypeNum k, AsPeano n ~ (S x), SPeanoI x) =>
         Enum (Periodic (n :: k)) where
    toEnum x =
        Periodic $
        fromJust $
        numToOrdinal $
        (fromIntegral x) `mod` (maxCoordSize (Proxy @(Periodic n)))
    fromEnum (Periodic o) = ordinalToNum o

instance (IsTypeNum k, SPeanoI (AsPeano n)) => IsCoord (Periodic (n :: k)) where
    type CoordSized (Periodic n) = AsPeano n
    asOrdinal = iso unPeriodic Periodic
    sCoordSized _ = sPeano
    maxCoordSize p = demoteSPeano (sCoordSized p) - 1

instance (AsPeano n ~ S x, IsTypeNum k, SPeanoI x) =>
         Semigroup (Periodic (n :: k)) where
    Periodic a <> Periodic b =
        let n = maxCoordSize (Proxy :: Proxy (Periodic n)) + 1
        in Periodic $
           fromJust $ numToOrdinal ((ordinalToNum a + ordinalToNum b) `mod` n)

instance (AsPeano n ~ (S x), SPeanoI x, IsTypeNum k) =>
         Monoid (Periodic (n :: k)) where
    mappend = (<>)
    mempty = Periodic $ fromJust $ numToOrdinal 0

instance (AsPeano n ~ (S x), SPeanoI x, IsTypeNum k) =>
         AdditiveGroup (Periodic (n :: k)) where
    zeroV = mempty
    (^+^) = (<>)
    negateV (Periodic o) =
        let n = fromIntegral (maxCoordSize (Proxy @ (Periodic n))) + 1
        in Periodic $ fromJust $ numToOrdinal (negate (ordinalToNum o) `mod` n)

instance (AsPeano n ~ (S x), SPeanoI x, IsTypeNum k) =>
         AffineSpace (Periodic (n :: k)) where
    type Diff (Periodic n) = Integer
    Periodic a .-. Periodic b =
        (ordinalToNum a - ordinalToNum b) `mod`
        (fromIntegral $ maxCoordSize (Proxy @(Periodic n)) + 1)
    Periodic a .+^ b =
        Periodic $
        fromJust $
        numToOrdinal $
        (fromIntegral (ordinalToNum a) + b) `mod`
        (fromIntegral $ maxCoordSize (Proxy @(Periodic n)) - 1)
