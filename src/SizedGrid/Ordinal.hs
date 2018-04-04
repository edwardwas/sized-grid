{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module SizedGrid.Ordinal where

import           Data.Constraint
import           Data.Constraint.Nat
import           Data.Maybe          (fromJust)
import           Data.Proxy
import           GHC.TypeLits
import           System.Random
import           Unsafe.Coerce

data SBool a where
  STrue :: SBool 'True
  SFalse :: SBool 'False

deriving instance Show (SBool a)

class SBoolI a where
  sBool :: SBool a

instance SBoolI 'True where
  sBool = STrue

instance SBoolI 'False where
  sBool = SFalse

sLessThan ::
       forall n m. (KnownNat n, KnownNat m)
    => Proxy n
    -> Proxy m
    -> SBool (n <=? m)
sLessThan _ _ =
    if natVal (Proxy @n) <= natVal (Proxy @m)
        then unsafeCoerce STrue
        else unsafeCoerce SFalse

data Ordinal m where
  Ordinal :: (KnownNat n, KnownNat m, (n + 1 <=? m) ~ 'True ) => Proxy n -> Ordinal m

instance Show (Ordinal m) where
  show (Ordinal p) = "Ordinal (" ++ show (natVal p) ++ "/" ++ show (natVal (Proxy @m)) ++ ")"

instance Eq (Ordinal m) where
  Ordinal a == Ordinal b = natVal a == natVal b

instance Ord (Ordinal m) where
  compare (Ordinal a) (Ordinal b) = compare (natVal a) (natVal b)

instance (1 <= m, KnownNat m) => Random (Ordinal m) where
    randomR (mi, ma) g =
        let (n, g') = randomR (fromEnum mi, fromEnum ma) g
        in (toEnum n, g')
    random = randomR (minBound, maxBound)

numToOrdinal ::
       forall a m. (KnownNat m, Integral a)
    => a
    -> Maybe (Ordinal m)
numToOrdinal n =
    case someNatVal (fromIntegral n) of
        Nothing -> Nothing
        Just (SomeNat (p :: Proxy n)) ->
            (case sLessThan (Proxy @ (n + 1)) (Proxy :: Proxy m) of
                SFalse -> Nothing
                STrue  -> Just $ Ordinal p) \\ plusNat @n @1

ordinalToNum :: Num a => Ordinal m -> a
ordinalToNum (Ordinal p) = fromIntegral $ natVal p

takeAddIsId :: forall m . Dict (((m - 1) + 1) ~ m)
takeAddIsId = unsafeCoerce (Dict :: Dict (a ~ a))

newtype Magic n = Magic (KnownNat n => Dict (KnownNat n))

magic ::
       forall n m o.
       (Integer -> Integer -> Integer)
    -> (KnownNat n, KnownNat m) :- KnownNat o
magic f =
    Sub $
    unsafeCoerce
        (Magic Dict)
        (natVal (Proxy :: Proxy n) `f` natVal (Proxy :: Proxy m))

takeNat :: (KnownNat n, KnownNat m) :- KnownNat (n - m)
takeNat = magic (-)

type OneOrHigher x = 1 <= x

instance (1 <= m, KnownNat m) => Bounded (Ordinal m) where
    minBound = Ordinal (Proxy @0)
    maxBound =
        Ordinal (Proxy @(m - 1)) \\
        (eqLe @((m - 1) + 1) @m `trans` Sub @() takeAddIsId) \\
        takeNat @m @1

instance (1 <= m, KnownNat m) => Enum (Ordinal m) where
  toEnum = fromJust . numToOrdinal
  fromEnum (Ordinal p) = fromIntegral $ natVal p
