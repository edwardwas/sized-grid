{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module SizedGrid.Peano where

import           Control.Lens
import           Data.Maybe
import           GHC.TypeLits

data Peano = Z | S Peano
  deriving (Eq,Show,Ord)

data SPeano (n :: Peano) where
  SZ :: SPeano Z
  SS :: SPeanoI n => SPeano n -> SPeano (S n)

type family NatToPeano (n :: Nat) = (x :: Peano) where
  NatToPeano 0 = Z
  NatToPeano n = S (NatToPeano (n - 1))

type family PeanoToNat (n :: Peano) :: Nat where
  PeanoToNat Z = 0
  PeanoToNat (S n) = 1 + PeanoToNat n

class SPeanoI (n :: Peano) where
  sPeano :: SPeano n

instance SPeanoI Z where
  sPeano = SZ

instance SPeanoI n => SPeanoI (S n) where
  sPeano = SS sPeano

withSPeano :: SPeano n -> (SPeanoI n => x) -> x
withSPeano SZ x     = x
withSPeano (SS n) x = x

demoteSPeano :: SPeano n -> Peano
demoteSPeano SZ     = Z
demoteSPeano (SS n) = S $ demoteSPeano n

promoteSPeano :: Peano -> (forall n . SPeanoI n => SPeano n -> x) -> x
promoteSPeano Z func     = func SZ
promoteSPeano (S n) func = promoteSPeano n (\n' -> func (SS n'))

peanoNegation :: Peano -> Peano -> Maybe Peano
peanoNegation a Z         = Just a
peanoNegation (S a) (S b) = peanoNegation a b
peanoNegation _ _         = Nothing

instance Num Peano where
    S n + x = n + S x
    Z + x = x
    a - b = fromJust $ peanoNegation a b
    x * Z = Z
    Z * x = Z
    a * S b = a + (a * b)
    negate Z = Z
    negate _ = error "Cannot negate Peano"
    abs = id
    signum _ = S Z
    fromInteger x
        | x < 0 = error ""
        | x == 0 = Z
        | x > 0 = S (fromInteger (x - 1))

type family PeanoAddition (a :: Peano) (b :: Peano) :: Peano where
  PeanoAddition (S n) x = S (PeanoAddition n x)
  PeanoAddition Z x = x

type family PeanoMultiplication (a :: Peano) (b :: Peano) :: Peano where
  PeanoMultiplication Z _ = Z
  PeanoMultiplication _ Z = Z
  PeanoMultiplication a (S b) = PeanoAddition a (PeanoMultiplication a b)

instance Real Peano where
  toRational Z     = 0
  toRational (S n) = 1 + toRational n

instance Enum Peano where
  succ = S
  pred (S n) = n
  pred Z     = error "Previous on Z"
  fromEnum Z     = 0
  fromEnum (S n) = 1 + fromEnum n
  toEnum x
    | x < 0 = error "To enum from negetaive peano"
    | x == 0 = Z
    | x > 0 = S (toEnum (x - 1))

instance Integral Peano where
    quotRem x y =
        let helper n a = case peanoNegation a y of
              Nothing -> (n, fromJust $ peanoNegation x (n * y))
              Just c  -> helper (n + 1) c
        in helper 0 x
    toInteger Z     = 0
    toInteger (S n) = 1 + toInteger n
    divMod = quotRem

