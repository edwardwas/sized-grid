{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module SizedGrid.Type.Number where

import           SizedGrid.Peano

import           GHC.Exts        (Constraint)
import qualified GHC.TypeLits    as GHC

class IsTypeNum k where
  type AsPeano (n :: k) :: Peano
  type AsNat (n :: k) :: GHC.Nat
  type Multiply (a :: k) (b :: k) :: k
  type Zero :: k
  type One :: k
  type Succ (n :: k) :: k
  type (<) (a :: k) (b :: k) :: Constraint

instance IsTypeNum GHC.Nat where
  type AsPeano n = NatToPeano n
  type AsNat n = n
  type Multiply a b = a GHC.* b
  type Zero = 0
  type One = 1
  type Succ n = (GHC.+) 1 n
  type (<) a b = GHC.CmpNat a b ~ LT

type family PeanoLessThan a b :: Constraint where
  PeanoLessThan Z (S n) = ()
  PeanoLessThan (S n) (S m) = PeanoLessThan n m

instance IsTypeNum Peano where
  type AsPeano n = n
  type AsNat n = PeanoToNat n
  type Multiply a b = PeanoMultiplication a b
  type Zero = Z
  type One = S Z
  type Succ n = S n
  type (<) a b = PeanoLessThan a b

type family Length xs where
  Length '[] = Zero
  Length (x ': xs) =  Succ (Length xs)

