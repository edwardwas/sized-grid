{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module SizedGrid.Coord.Class where

import           SizedGrid.Ordinal

import           Control.Lens
import           Data.Constraint
import           Data.Proxy
import           GHC.TypeLits
import           Unsafe.Coerce     (unsafeCoerce)

-- | Proof an idiom about how `CoordFromNat` works. This relies on 'CoordFromNat a (CoordSized a ~ a'
coordFromNatCollapse ::
       forall a x y. Dict (CoordFromNat (CoordFromNat a x) y ~ CoordFromNat a y)
coordFromNatCollapse = unsafeCoerce (Dict :: Dict (z ~ z))

coordFromNatSame ::
       (CoordFromNat a ~ CoordFromNat b) :- (a ~ CoordFromNat b (CoordSized a))
coordFromNatSame = Sub (unsafeCoerce (Dict :: Dict (a ~ a)))

coordSizedCollapse :: forall c n . Dict (CoordSized (CoordFromNat c n) ~ n)
coordSizedCollapse = unsafeCoerce (Dict :: Dict (a ~ a))

-- | Everything that can be uses as a Coordinate. The only required function is `asOrdinal` and the type instance of `CoordSized`: the rest can be derived automatically.
--
-- This is kind * -> Constraint for ease of use later. There is some argument that it should be of kind (Nat -> *) -> Constraint and we could remove `CoordSized`, but that has other complications
class (1 <= CoordSized c, KnownNat (CoordSized c))  => IsCoord c where
  -- | The maximum number of values that a Coord can take
  type CoordSized c :: Nat
  type CoordFromNat c :: (Nat -> *)
  -- | As each coord represents a finite number of states, it must be isomorphic to an Ordinal
  asOrdinal :: Iso' c (Ordinal (CoordSized c))
  -- | The origin. If c is an instance of `Monoid`, this should be mempty
  zeroPosition :: c
  default zeroPosition :: Monoid c => c
  zeroPosition = mempty
  -- | Retrive a `Proxy` of the size
  sCoordSized :: proxy c -> Proxy (CoordSized c)
  sCoordSized _ = Proxy
  -- | The largest possible number expressable
  maxCoordSize :: proxy c -> Integer
  maxCoordSize p = natVal (sCoordSized p) - 1

  asSizeProxy ::
         c
      -> (forall n. (KnownNat n, n + 1 <= (CoordSized c)) =>
                        Proxy n -> x)
      -> x
  asSizeProxy c = asSizeProxy (view asOrdinal c)

  weakenIsCoord :: IsCoord (CoordFromNat c n) => c -> Maybe (CoordFromNat c n)
  weakenIsCoord = fmap (review asOrdinal) . weakenOrdinal . view asOrdinal

  strengthenIsCoord ::
       (IsCoord (CoordFromNat c n), CoordSized c <= CoordSized (CoordFromNat c n))
    => c
    -> CoordFromNat c n
  strengthenIsCoord = review asOrdinal . strengthenOrdinal . view asOrdinal

instance (1 <= n, KnownNat n) => IsCoord (Ordinal n) where
    type CoordSized (Ordinal n) = n
    type CoordFromNat (Ordinal n) = Ordinal
    asOrdinal = id
    zeroPosition = Ordinal (Proxy @0)
    asSizeProxy (Ordinal p) func = func p

-- | Enumerate all possible values of a coord, in order
allCoordLike :: IsCoord c => [c]
allCoordLike = toListOf (traverse . re asOrdinal) [minBound .. maxBound]
