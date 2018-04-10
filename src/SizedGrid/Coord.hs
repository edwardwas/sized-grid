{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module SizedGrid.Coord where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal

import           Control.Applicative   (liftA2)
import           Control.Applicative   (empty)
import           Control.Lens          ((^.))
import           Control.Monad.State
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           Data.Functor.Identity
import           Data.List             (intercalate)
import           Data.Semigroup        (Semigroup (..))
import qualified Data.Vector           as V
import           Generics.SOP          hiding (Generic, S, Z)
import qualified Generics.SOP          as SOP
import           GHC.Exts              (Constraint)
import           GHC.Generics          (Generic)
import qualified GHC.TypeLits          as GHC
import           System.Random         (Random (..))

-- | Length of a type level list
type family Length cs where
  Length '[] = 0
  Length (c ': cs) = (GHC.+) 1 (Length cs)

-- | A multideminsion coordinate
newtype Coord cs = Coord {unCoord :: NP I cs}
  deriving (Generic)

instance All Eq cs => Eq (Coord cs) where
    Coord a == Coord b =
        and $
        hcollapse $ hcliftA2 (Proxy :: Proxy Eq) (\(I x) (I y) -> K (x == y)) a b

instance (All Eq cs, All Ord cs) => Ord (Coord cs) where
    compare (Coord a) (Coord b) =
        mconcat $
        hcollapse $
        hcliftA2 (Proxy :: Proxy Ord) (\(I x) (I y) -> K (compare x y)) a b

instance All Show cs => Show (Coord cs) where
    show (Coord a) =
        "Coord [" ++
        intercalate
            ", "
            (hcollapse $ hcliftA (Proxy :: Proxy Show) (\(I x) -> K $ show x) a) ++
        "]"

instance (All ToJSON cs) => ToJSON (Coord cs) where
    toJSON (Coord a) =
        Array $
        V.fromList $
        hcollapse $ hcmap (Proxy @ToJSON) (\(I x) -> K $ toJSON x) a

instance All FromJSON cs => FromJSON (Coord cs) where
    parseJSON =
        withArray "Coord" $ \v ->
            case SOP.fromList $ V.toList v of
                Just a ->
                    Coord <$>
                    hsequence
                        (hcmap (Proxy @FromJSON) (\(K x) -> parseJSON x) a)
                Nothing -> empty


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
type instance CoordDiff '[a, b] = (Diff a, Diff b)
type instance CoordDiff '[a, b, c] = (Diff a, Diff b, Diff c)
type instance CoordDiff '[a, b, c, d] =
     (Diff a, Diff b, Diff c, Diff d)
type instance CoordDiff '[a, b, c, d, e] =
     (Diff a, Diff b, Diff c, Diff d, Diff e)
type instance CoordDiff '[a, b, c, d, e, f] =
     (Diff a, Diff b, Diff c, Diff d, Diff e, Diff f)

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
            helper (I x :* xs) (I y :* ys) = I (x .-. y) :* helper xs ys
        in to $ SOP $ SOP.Z $ helper a b
    Coord a .+^ b =
        let helper :: All AffineSpace xs => NP I xs -> NP I (MapDiff xs) -> NP I xs
            helper Nil Nil                 = Nil
            helper (I x :* xs) (I y :* ys) = I (x .+^ y) :* helper xs ys
        in case from b of
              SOP (SOP.Z bs) -> Coord $ helper a bs
              _ -> error "Error in adding Coord. Should be unreachable"

allCoord ::
       forall cs. (All IsCoord cs)
    => [Coord cs]
allCoord = Coord <$> hsequence (hcpure (Proxy :: Proxy IsCoord) allCoordLike)

type family MaxCoordSize (cs :: [k]) :: GHC.Nat where
  MaxCoordSize '[] = 1
  MaxCoordSize (c ': cs) = (CoordSized c) GHC.* (MaxCoordSize cs)

coordPosition :: (All IsCoord cs) => Coord cs -> Int
coordPosition (Coord a) =
    let helper :: (All IsCoord xs) => NP I xs -> Integer
        helper Nil = 0
        helper (I c :* (cs :: NP I ys)) =
            ordinalToNum (c ^. asOrdinal) * sizeOfList cs + helper cs
        sizeOfList :: All IsCoord xs => NP I xs -> Integer
        sizeOfList =
            product .
            hcollapse .
            hcmap
                (Proxy :: Proxy IsCoord)
                (\(I (_ :: a)) -> K $ 1 + maxCoordSize (Proxy :: Proxy a))
    in fromIntegral $ helper a

type family AllDiffSame a xs :: Constraint where
  AllDiffSame _ '[] = ()
  AllDiffSame a (x ': xs) = (Diff x ~ a, AllDiffSame a xs)

--
moorePoints ::
     forall a cs. (Enum a, Num a, AllDiffSame a cs, All AffineSpace cs)
  => a
  -> Coord cs
  -> [Coord cs]
moorePoints n (Coord cs) =
  let helper :: (All AffineSpace xs, AllDiffSame a xs) => NP I xs -> [NP I xs]
      helper Nil = [Nil]
      helper (I a :* as) = do
        delta :: a <- [-n .. n]
        next <- helper as
        return (I (a .+^ delta) :* next)
  in map Coord $ helper cs

vonNeumanPoints ::
     forall a cs.
     ( Enum a
     , Num a
     , Ord a
     , All Integral (MapDiff cs)
     , AllDiffSame a cs
     , All AffineSpace cs
     , Ord (CoordDiff cs)
     , IsProductType (CoordDiff cs) (MapDiff cs)
     , AdditiveGroup (CoordDiff cs)
     )
  => a
  -> Coord cs
  -> [Coord cs]
vonNeumanPoints n c =
    let helper :: Coord cs -> Bool
        helper new =
            sum
                (hcollapse $
                 hcmap
                     (Proxy :: Proxy Integral)
                     (\(I a) -> K (abs $ fromIntegral a)) $
                 from (min (new .-. c) (c .-. new))) <= n
    in filter helper $ moorePoints n c
