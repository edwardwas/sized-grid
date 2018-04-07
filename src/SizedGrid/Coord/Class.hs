{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module SizedGrid.Coord.Class where

import           SizedGrid.Ordinal

import           Control.Lens
import           Data.Proxy
import           GHC.TypeLits

class (1 <= CoordSized c, KnownNat (CoordSized c)) => IsCoord c where
  type CoordSized c :: Nat
  asOrdinal :: Iso' c (Ordinal (CoordSized c))
  sCoordSized :: proxy c -> Proxy (CoordSized c)
  sCoordSized _ = Proxy
  maxCoordSize :: proxy c -> Integer
  maxCoordSize p = natVal (sCoordSized p) - 1

instance (1 <= n, KnownNat n) => IsCoord (Ordinal n) where
    type CoordSized (Ordinal n) = n
    asOrdinal = id

overOrdinal ::
       IsCoord c
    => (Ordinal (CoordSized c) -> Ordinal (CoordSized c))
    -> c
    -> c
overOrdinal func = over asOrdinal func

allCoordLike :: (IsCoord c, 1 <= CoordSized c) => [c]
allCoordLike = toListOf (traverse . re asOrdinal) [minBound .. maxBound]
