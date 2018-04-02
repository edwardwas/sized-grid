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

newtype Periodic (n :: Peano) = Periodic
    { unPeriodic :: Ordinal n
    } deriving (Eq,Show,Ord)

deriving instance (n ~ (S x), SPeanoI x) => Random (Periodic n)

instance (n ~ (S x), SPeanoI x) => Enum (Periodic (n)) where
    toEnum x =
        Periodic $
        fromJust $
        numToOrdinal $
        (fromIntegral x) `mod` (maxCoordSize (Proxy @(Periodic n)))
    fromEnum (Periodic o) = ordinalToNum o

instance (SPeanoI n) => IsCoord (Periodic n) where
    type CoordSized (Periodic n) = n
    asOrdinal = iso unPeriodic Periodic
    sCoordSized _ = sPeano
    maxCoordSize p = demoteSPeano (sCoordSized p) - 1

instance (n ~ S x, SPeanoI x) => Semigroup (Periodic n) where
    Periodic a <> Periodic b =
        let n = maxCoordSize (Proxy :: Proxy (Periodic n)) + 1
        in Periodic $
           fromJust $ numToOrdinal ((ordinalToNum a + ordinalToNum b) `mod` n)

instance (n ~ (S x), SPeanoI x) => Monoid (Periodic n) where
    mappend = (<>)
    mempty = Periodic OZ

instance (n ~ (S x), SPeanoI x) => AdditiveGroup (Periodic n) where
    zeroV = mempty
    (^+^) = (<>)
    negateV (Periodic o) =
        let n = fromIntegral (maxCoordSize (Proxy @(Periodic n))) + 1
        in Periodic $ fromJust $ numToOrdinal (negate (ordinalToNum o) `mod` n)

instance (n ~ (S x), SPeanoI x) => AffineSpace (Periodic n) where
    type Diff (Periodic n) = Integer
    Periodic a .-. Periodic b =
        (ordinalToNum a - ordinalToNum b) `mod`
        (fromIntegral $ maxCoordSize (Proxy @(Periodic n)) + 1)
    Periodic a .+^ b =
        Periodic $
        fromJust $
        numToOrdinal $
        (fromIntegral (ordinalToNum a) + b) `mod`
        (fromIntegral $ maxCoordSize (Proxy @(Periodic n)) + 1)
