{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module SizedGrid.Type.Number where

import           SizedGrid.Peano

import qualified GHC.TypeLits    as GHC

class IsTypeNum k where
  type AsPeano (n :: k) :: Peano
  type AsNat (n :: k) :: GHC.Nat
  type Multiply (a :: k) (b :: k) :: k
  type Zero :: k
  type One :: k

instance IsTypeNum GHC.Nat where
  type AsPeano n = NatToPeano n
  type AsNat n = n
  type Multiply a b = a GHC.* b
  type Zero = 0
  type One = 1

instance IsTypeNum Peano where
  type AsPeano n = n
  type AsNat n = PeanoToNat n
  type Multiply a b = PeanoMultiplication a b
  type Zero = Z
  type One = S Z
