{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}

module SizedGrid.Ordinal where

import           SizedGrid.Peano

import           Data.Maybe      (fromJust)
import           GHC.TypeLits
import           System.Random

data Ordinal n where
  OZ :: Ordinal (S n)
  OS :: Ordinal n -> Ordinal (S n)

deriving instance Eq (Ordinal n)
deriving instance Show (Ordinal n)
deriving instance Ord (Ordinal n)

maxOrdinal :: forall n . SPeanoI n => Ordinal (S n)
maxOrdinal = case (sPeano :: SPeano n) of
  SZ   -> OZ
  SS n -> OS maxOrdinal

instance SPeanoI n => Bounded (Ordinal (S n)) where
  minBound = OZ
  maxBound = maxOrdinal

instance SPeanoI n => Enum (Ordinal (S n)) where
  toEnum = fromJust . numToOrdinal
  fromEnum = ordinalToNum
  pred (OS n) = weakenOrdinal n
  pred OZ     = error "Pred on OZ"
  succ n = case strengthenOrdinal (OS n) of
      Just x  -> x
      Nothing -> error "Succ on maxOrdinal"

instance SPeanoI n => Random (Ordinal (S n)) where
    random g =
        let xs = [minBound .. maxBound]
            (n, g') = randomR (0, length xs - 1) g
        in (xs !! n, g')
    randomR (mi, ma) g =
        let xs = [mi .. ma]
            (n,g') = randomR (0,length xs - 1) g
        in (xs !! n, g')

ordinalToNum :: Num a => Ordinal n -> a
ordinalToNum OZ     = 0
ordinalToNum (OS n) = 1 + ordinalToNum n

numToOrdinal :: forall a n . (SPeanoI n, Ord a, Num a) => a -> Maybe (Ordinal (S n))
numToOrdinal n
  | n < 0 = Nothing
  | n == 0 = Just OZ
  | n > 0 = case sPeano :: SPeano n of
      SS x -> withSPeano x $ OS <$> (numToOrdinal (n - 1))
      _    -> Nothing

weakenOrdinal :: Ordinal n -> Ordinal (S n)
weakenOrdinal OZ     = OZ
weakenOrdinal (OS n) = OS (weakenOrdinal n)

strengthenOrdinal ::
       forall n. SPeanoI n
    => Ordinal (S n)
    -> Maybe (Ordinal n)
strengthenOrdinal OZ =
    case (sPeano :: SPeano n) of
        SZ   -> Nothing
        SS _ -> Just OZ
strengthenOrdinal (OS n) = case (sPeano :: SPeano n) of
      SZ   -> Nothing
      SS _ -> OS <$> strengthenOrdinal n
