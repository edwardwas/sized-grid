{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module SizedGrid.Internal.Type where

import           Data.Constraint
import           Data.Proxy
import           GHC.TypeLits
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
