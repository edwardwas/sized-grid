{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module SizedGrid.Coord.Class where

import           SizedGrid.Ordinal

import           Control.Lens
import           Data.Proxy
import           GHC.TypeLits

-- | Everything that can be uses as a Coordinate. The only required function is `asOrdinal` and the type instance of `CoordSized`: the rest can be derived automatically.
--
-- This is kind * -> Constraint for ease of use later. There is some argument that it should be of kind (Nat -> *) -> Constraint and we could remove `CoordSized`, but that has other complications
class (1 <= CoordSized c, KnownNat (CoordSized c)) => IsCoord c where
  -- | The maximum number of values that a Coord can take
  type CoordSized c :: Nat
  -- | As each coord represents a finite number of states, it must be isomorphic to an Ordinal
  asOrdinal :: Iso' c (Ordinal (CoordSized c))
  -- | The origin. If c is an instance of `Monoid`, this should be mempty
  zeroPosition :: c
  default zeroPosition :: Monoid c => c
  zeroPosition = mempty
  -- | Retrive a `Proxy` of the size
  sCoordSized :: proxy c -> Proxy (CoordSized c)
  sCoordSized _ = Proxy
  -- | The largest possible number expressable
  maxCoordSize :: proxy c -> Integer
  maxCoordSize p = natVal (sCoordSized p) - 1

instance (1 <= n, KnownNat n) => IsCoord (Ordinal n) where
    type CoordSized (Ordinal n) = n
    asOrdinal = id
    zeroPosition = Ordinal (Proxy @0)

-- | Enumerate all possible values of a coord, in order
allCoordLike :: IsCoord c => [c]
allCoordLike = toListOf (traverse . re asOrdinal) [minBound .. maxBound]
