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

import           GHC.TypeLits

data Peano = Z | S Peano
  deriving (Eq,Show,Ord)

data SPeano (n :: Peano) where
  SZ :: SPeano Z
  SS :: SPeanoI n => SPeano n -> SPeano (S n)

type family NatToPeano (n :: Nat) :: Peano where
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

class IsPositive (n :: Peano)
instance (n ~ (S x), SPeanoI n) => IsPositive n
