{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module SizedGrid.Coord.Class where

import           SizedGrid.Ordinal
import           SizedGrid.Peano
import           SizedGrid.Type.Number

import           Control.Lens
import           GHC.TypeLits

class IsCoord c where
  type CoordSized c :: Peano
  asOrdinal :: Iso' c (Ordinal (CoordSized c))
  sCoordSized :: proxy c -> SPeano (CoordSized c)
  maxCoordSize :: proxy c -> Peano

overOrdinal ::
       IsCoord c
    => (Ordinal (AsPeano (CoordSized c)) -> Ordinal (AsPeano (CoordSized c)))
    -> c
    -> c
overOrdinal func = over asOrdinal func

allCoordLike :: (IsCoord c, AsPeano (CoordSized c) ~ S x, SPeanoI x) => [c]
allCoordLike = toListOf (traverse . re asOrdinal) [minBound .. maxBound]
