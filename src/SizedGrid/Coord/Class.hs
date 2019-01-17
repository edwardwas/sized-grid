{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module SizedGrid.Coord.Class where

import           SizedGrid.Ordinal

import           Control.Lens
import           Data.Maybe        (fromJust)
import           Data.Proxy
import           GHC.TypeLits

---- | Proof an idiom about how `CoordFromNat` works. This relies on 'CoordFromNat a (CoordSized a ~ a'
--coordFromNatCollapse ::
--       forall a x y. Dict (CoordFromNat (CoordFromNat a x) y ~ CoordFromNat a y)
--coordFromNatCollapse = unsafeCoerce (Dict :: Dict (z ~ z))

--coordFromNatSame ::
--       (CoordFromNat a ~ CoordFromNat b) :- (a ~ CoordFromNat b (CoordSized a))
--coordFromNatSame = Sub (unsafeCoerce (Dict :: Dict (a ~ a)))
--
--coordSizedCollapse :: forall c n . Dict (CoordSized (CoordFromNat c n) ~ n)
--coordSizedCollapse = unsafeCoerce (Dict :: Dict (a ~ a))

---- | Everything that can be uses as a Coordinate. The only required function is `asOrdinal` and the type instance of `CoordSized`: the rest can be derived automatically.
----
---- This is kind * -> Constraint for ease of use later. There is some argument that it should be of kind (Nat -> *) -> Constraint and we could remove `CoordSized`, but that has other complications
--class (1 <= CoordSized c, KnownNat (CoordSized c))  => IsCoord c where
--  -- | The maximum number of values that a Coord can take
--  type CoordSized c :: Nat
--  type CoordFromNat c :: (Nat -> *)
--  -- | As each coord represents a finite number of states, it must be isomorphic to an Ordinal
--  asOrdinal :: Iso' c (Ordinal (CoordSized c))
--  -- | The origin. If c is an instance of `Monoid`, this should be mempty
--  zeroPosition :: c
--  default zeroPosition :: Monoid c => c
--  zeroPosition = mempty
--  -- | Retrive a `Proxy` of the size
--  sCoordSized :: proxy c -> Proxy (CoordSized c)
--  sCoordSized _ = Proxy
--  -- | The largest possible number expressable
--  maxCoordSize :: proxy c -> Integer
--  maxCoordSize p = natVal (sCoordSized p) - 1
--
--  maxCoord :: c
--  maxCoord = view (re asOrdinal) maxCoord
--
--  asSizeProxy ::
--         c
--      -> (forall n. (KnownNat n, n + 1 <= (CoordSized c)) =>
--                        Proxy n -> x)
--      -> x
--  asSizeProxy c = asSizeProxy (view asOrdinal c)
--
--  weakenIsCoord :: IsCoord (CoordFromNat c n) => c -> Maybe (CoordFromNat c n)
--  weakenIsCoord = fmap (review asOrdinal) . weakenOrdinal . view asOrdinal
--
--  strengthenIsCoord ::
--       (IsCoord (CoordFromNat c n), CoordSized c <= CoordSized (CoordFromNat c n))
--    => c
--    -> CoordFromNat c n
--  strengthenIsCoord = review asOrdinal . strengthenOrdinal . view asOrdinal

class IsCoord (c :: Nat -> *) where
  asOrdinal :: Iso' (c n) (Ordinal n)

  zeroPosition :: (1 <= n, KnownNat n) => c n
  default zeroPosition :: Monoid (c n) => c n
  zeroPosition = mempty

  sCoordSized :: Proxy (c n) -> Proxy n
  sCoordSized _ = Proxy

  maxCoordSize :: KnownNat n => Proxy (c n) -> Integer
  maxCoordSize p = natVal (sCoordSized p) - 1

  maxCoord :: KnownNat n => Proxy n -> c n
  maxCoord _ = view (re asOrdinal) (maxCoord (Proxy :: Proxy n))

  asSizeProxy ::
         c n
      -> (forall m. (KnownNat m, m + 1 <= n) =>
                        Proxy m -> x)
      -> x
  asSizeProxy c = asSizeProxy (view asOrdinal c)

  weakenIsCoord :: KnownNat m => c n -> Maybe (c m)
  weakenIsCoord = fmap (review asOrdinal) . weakenOrdinal . view asOrdinal

  strengthenIsCoord :: (KnownNat m, (n <= m)) => c n -> c m
  strengthenIsCoord = review asOrdinal . strengthenOrdinal . view asOrdinal

class ( x ~ ((CoordContainer x) (CoordNat x))
      , 1 <= CoordNat x
      , IsCoord (CoordContainer x)
      , KnownNat (CoordNat x)
      ) =>
      IsCoordLifted x
    where
    type CoordContainer x :: Nat -> *
    type CoordNat x :: Nat

instance (KnownNat n, 1 <= n, IsCoord c) => IsCoordLifted (c n) where
  type CoordContainer (c n) = c
  type CoordNat (c n) = n

instance IsCoord Ordinal where
    asOrdinal = id
    zeroPosition = Ordinal (Proxy @0)
    asSizeProxy (Ordinal p) func = func p
    maxCoord :: forall n proxy . KnownNat n => proxy n -> Ordinal n
    maxCoord _ = fromJust $ numToOrdinal (maxCoordSize (Proxy :: Proxy (Ordinal n)))

-- | Enumerate all possible values of a coord, in order
allCoordLike :: (1 <= n, IsCoord c, KnownNat n) => [c n]
allCoordLike = toListOf (traverse . re asOrdinal) [minBound .. maxBound]
