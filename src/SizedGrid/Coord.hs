{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module SizedGrid.Coord where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal
import           SizedGrid.Peano

import           Control.Applicative      (liftA2)
import           Control.Lens             ((^.), _1)
import           Control.Monad.State
import           Control.Newtype.Generics
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Functor.Identity
import           Data.Semigroup           (Semigroup (..))
import           Generics.SOP             hiding (Generic, S, Z)
import qualified Generics.SOP             as SOP
import           GHC.Exts                 (Constraint)
import           GHC.Generics             (Generic)
import qualified GHC.TypeLits             as GHC
import           System.Random

newtype Coord cs = Coord {unCoord :: NP I cs}
  deriving (Eq, Show, Generic)

instance Newtype (Coord cs)

instance All Semigroup cs => Semigroup (Coord cs) where
  Coord a <> Coord b = Coord $ hcliftA2 (Proxy :: Proxy Semigroup) (liftA2 (<>)) a b

instance (All Semigroup cs, All Monoid cs) => Monoid (Coord cs) where
  mappend = (<>)
  mempty = Coord $ hcpure (Proxy :: Proxy Monoid) (pure mempty)

instance (All AdditiveGroup cs) => AdditiveGroup (Coord cs) where
    zeroV = Coord $ hcpure (Proxy :: Proxy AdditiveGroup) (pure zeroV)
    Coord a ^+^ Coord b =
        Coord $ hcliftA2 (Proxy :: Proxy AdditiveGroup) (liftA2 (^+^)) a b
    negateV (Coord a) =
        Coord $ hcliftA (Proxy :: Proxy AdditiveGroup) (fmap negateV) a
    Coord a ^-^ Coord b =
        Coord $ hcliftA2 (Proxy :: Proxy AdditiveGroup) (liftA2 (^-^)) a b

instance (All Random cs) => Random (Coord cs) where
    random g =
        let (c, g') =
                runState
                    (hsequence $ hcpure (Proxy :: Proxy Random) (state random))
                    g
        in (Coord c, g')
    randomR (Coord mi, Coord ma) g =
        let (c, g') =
                runState
                    (hsequence $
                     hcliftA2
                         (Proxy :: Proxy Random)
                         (\(I a) (I b) -> state (randomR (a, b)))
                         mi
                         ma)
                    g
        in (Coord c, g')

type family CoordDiff (cs :: [k]) :: *

type instance CoordDiff '[] = ()
type instance CoordDiff '[a] = Identity (Diff a)
type instance CoordDiff '[a,b] = (Diff a, Diff b)
type instance CoordDiff '[a,b,c] = (Diff a, Diff b, Diff c)
type instance CoordDiff '[a,b,c,d] = (Diff a, Diff b, Diff c, Diff d)

type family MapDiff xs where
  MapDiff '[] = '[]
  MapDiff (x ': xs) = Diff x ': MapDiff xs

instance ( All AffineSpace cs
         , AdditiveGroup (CoordDiff cs)
         , IsProductType (CoordDiff cs) (MapDiff cs)
         ) =>
         AffineSpace (Coord cs) where
    type Diff (Coord cs) = CoordDiff cs
    Coord a .-. Coord b =
        let helper ::
                   All AffineSpace xs => NP I xs -> NP I xs -> NP I (MapDiff xs)
            helper Nil Nil                 = Nil
            helper (I a :* as) (I b :* bs) = I (a .-. b) :* helper as bs
        in to $ SOP $ SOP.Z $ helper a b
    Coord a .+^ b =
        let helper :: All AffineSpace xs => NP I xs -> NP I (MapDiff xs) -> NP I xs
            helper Nil Nil                 = Nil
            helper (I a :* as) (I b :* bs) = I (a .+^ b) :* helper as bs
        in case from b of
              SOP (SOP.Z bs) -> Coord $ helper a bs

allCoord ::
       forall cs. (All IsCoord cs, SListI cs, All Monoid cs, All Semigroup cs)
    => [Coord cs]
allCoord =
    let helper ::
               forall xs. (All IsCoord xs, SListI xs)
            => NP I xs
            -> [NP I xs]
        helper Nil = [Nil]
        helper (I (_ :: x) :* ns) =
            case sCoordSized (Proxy :: Proxy x) of
                SS _ -> do
                    cs <- helper ns
                    c <- allCoordLike
                    return (I c :* cs)
    in map Coord $ helper $ unCoord mempty

type family MaxCoordSize cs where
  MaxCoordSize '[] = 1
  MaxCoordSize (c ': cs) = CoordSized c GHC.* MaxCoordSize cs

coordPosition :: (All IsCoord cs) => Coord cs -> Int
coordPosition (Coord a) =
    let helper :: (All IsCoord xs) => NP I xs -> Int
        helper (Nil) = 0
        helper (I c :* (cs :: NP I ys)) =
            ordinalToNum (c ^. asOrdinal) + ((maxCoordSize $ I c) * helper cs)
    in (helper a)
