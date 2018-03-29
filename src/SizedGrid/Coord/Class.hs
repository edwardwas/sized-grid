{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module SizedGrid.Coord.Class where

import           SizedGrid.Ordinal
import           SizedGrid.Peano

import           Control.Lens
import           GHC.TypeLits

class IsCoord c where
  type CoordSized c :: Nat
  asOrdinal :: Iso' c (Ordinal (NatToPeano (CoordSized c)))

overOrdinal ::
       IsCoord c
    => (Ordinal (NatToPeano (CoordSized c)) -> Ordinal (NatToPeano (CoordSized c)))
    -> c
    -> c
overOrdinal func = over asOrdinal func

allCoord :: (IsCoord c, NatToPeano (CoordSized c) ~ S x, SPeanoI x) => [c]
allCoord = toListOf (traverse . re asOrdinal) allOrdinal
