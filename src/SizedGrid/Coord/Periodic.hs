{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module SizedGrid.Coord.Periodic where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal
import           SizedGrid.Peano

import           Control.Lens
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Maybe            (fromJust)
import           Data.Proxy
import           Data.Semigroup
import           GHC.TypeLits
import           System.Random

newtype Periodic n = Periodic
    { unPeriodic :: Ordinal (NatToPeano n)
    } deriving (Eq,Show,Ord)

deriving instance (NatToPeano n ~ (S x), SPeanoI x) => Random (Periodic n)

instance (NatToPeano n ~ (S x), SPeanoI x, KnownNat n) =>
         Enum (Periodic n) where
  toEnum x =
    Periodic $
    fromJust $ numToOrdinal $ x `mod` (fromIntegral $ natVal (Proxy :: Proxy n))
  fromEnum (Periodic o) = ordinalToNum o

instance (SPeanoI (NatToPeano n), KnownNat n) => IsCoord (Periodic n) where
  type CoordSized (Periodic n) = n
  asOrdinal = iso unPeriodic Periodic
  sCoordSized _ = sPeano
  maxCoordSize _ = fromIntegral $ natVal (Proxy :: Proxy n)

instance (NatToPeano n ~ (S x), SPeanoI x, KnownNat n) =>
         Semigroup (Periodic n) where
    Periodic a <> Periodic b =
        let n = natVal (Proxy :: Proxy n)
        in Periodic $
           fromJust $ numToOrdinal ((ordinalToNum a + ordinalToNum b) `mod` n)

instance (NatToPeano n ~ (S x), SPeanoI x, KnownNat n) =>
         Monoid (Periodic n) where
    mappend = (<>)
    mempty = Periodic $ fromJust $ numToOrdinal 0

instance (NatToPeano n ~ (S x), SPeanoI x, KnownNat n) =>
         AdditiveGroup (Periodic n) where
    zeroV = mempty
    (^+^) = (<>)
    negateV (Periodic o) =
        Periodic $
        fromJust $
        numToOrdinal $
        (negate $ ordinalToNum o) `mod` (natVal (Proxy :: Proxy n))

instance (NatToPeano n ~ (S x), SPeanoI x, KnownNat n) =>
         AffineSpace (Periodic n) where
    type Diff (Periodic n) = Integer
    Periodic a .-. Periodic b =
        (ordinalToNum a - ordinalToNum b) `mod` (natVal (Proxy :: Proxy n))
    Periodic a .+^ b =
        Periodic $
        fromJust $
        numToOrdinal $ (ordinalToNum a + b) `mod` (natVal (Proxy :: Proxy n))
