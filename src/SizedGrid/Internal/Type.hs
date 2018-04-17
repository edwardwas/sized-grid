{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      :  SizedGrid.Internal.Type
-- Copyright   :  (C) 2018-18 Edward Wastell
-- License     :  MIT -style (see the file LICENSE)
-- Maintainer  :  Edward Wastell <ed@wastell.co.uk>
-- Stability   :  provisional

module SizedGrid.Internal.Type where

import           Data.Constraint
import           Data.Proxy
import           GHC.TypeLits
import           Unsafe.Coerce

-- | A singleton type for Bools
data SBool a where
  STrue :: SBool 'True
  SFalse :: SBool 'False

deriving instance Show (SBool a)

-- | A type constraint for getting `SingI`
class SBoolI a where
  sBool :: SBool a

instance SBoolI 'True where
  sBool = STrue

instance SBoolI 'False where
  sBool = SFalse

-- | Give a runtime representation of a type level number being less than or equal than another
sLessThan ::
       forall n m. (KnownNat n, KnownNat m)
    => Proxy n
    -> Proxy m
    -> SBool (n <=? m)
sLessThan _ _ =
    if natVal (Proxy @n) <= natVal (Proxy @m)
        then unsafeCoerce STrue
        else unsafeCoerce SFalse

-- | A Dict prove that m - 1 + 1 is m
takeAddIsId :: forall m . Dict (((m - 1) + 1) ~ m)
takeAddIsId = unsafeCoerce (Dict :: Dict (a ~ a))

-- | Magic is stole from Constraints, and I don't really understand it, but it is needed for 'takeNat'
newtype Magic n = Magic (KnownNat n => Dict (KnownNat n))

-- | Also don't understand
magic ::
       forall n m o.
       (Integer -> Integer -> Integer)
    -> (KnownNat n, KnownNat m) :- KnownNat o
magic f =
    Sub $
    unsafeCoerce
        (Magic Dict)
        (natVal (Proxy :: Proxy n) `f` natVal (Proxy :: Proxy m))

-- | Runtime proof that n - m is an insance of KnownNat if n and m are
takeNat :: (KnownNat n, KnownNat m) :- KnownNat (n - m)
takeNat = magic (-)
