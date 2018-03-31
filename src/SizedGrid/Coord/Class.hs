{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module SizedGrid.Coord.Class where

import           SizedGrid.Ordinal
import           SizedGrid.Peano

import           Control.Lens
import           GHC.TypeLits

class (SPeanoI (NatToPeano (CoordSized c))) => IsCoord c where
  type CoordSized c :: Nat
  asOrdinal :: Iso' c (Ordinal (NatToPeano (CoordSized c)))
  sCoordSized :: proxy c -> SPeano (NatToPeano (CoordSized c))
  maxCoordSize :: proxy c -> Int

overOrdinal ::
       IsCoord c
    => (Ordinal (NatToPeano (CoordSized c)) -> Ordinal (NatToPeano (CoordSized c)))
    -> c
    -> c
overOrdinal func = over asOrdinal func

allCoordLike :: (IsCoord c, NatToPeano (CoordSized c) ~ S x, SPeanoI x) => [c]
allCoordLike = toListOf (traverse . re asOrdinal) [minBound .. maxBound]
