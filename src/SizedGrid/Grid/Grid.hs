{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module SizedGrid.Grid.Grid where

import           SizedGrid.Coord
import           SizedGrid.Coord.Class

import           Control.Lens          hiding (index)
import           Data.Aeson
import           Data.Constraint
import           Data.Distributive
import           Data.Functor.Classes
import           Data.Functor.Rep
import           Data.Proxy            (Proxy (..))
import           Data.Type.Equality
import qualified Data.Vector           as V
import           Generics.SOP
import qualified GHC.Generics          as GHC
import           GHC.TypeLits
import qualified GHC.TypeLits          as GHC

-- | A multi dimensional sized grid
newtype Grid (cs :: [*]) a = Grid
  { unGrid :: V.Vector a
  } deriving (Eq, Show, Functor, Foldable, Traversable, Eq1, Show1, GHC.Generic)

instance AllSizedKnown cs => Applicative (Grid cs) where
    pure =
        withDict
            (sizeProof @cs)
            (Grid .
             V.replicate
                 (fromIntegral $ GHC.natVal (Proxy :: Proxy (MaxCoordSize cs))))
    Grid fs <*> Grid as = Grid $ V.zipWith ($) fs as

instance (AllSizedKnown cs, All IsCoord cs) =>
         Monad (Grid cs) where
  g >>= f = imap (\p a -> f a `index` p) g

instance (AllSizedKnown cs, All IsCoord cs) =>
         Distributive (Grid cs) where
  distribute = distributeRep

instance (All IsCoord cs, AllSizedKnown cs) =>
         Representable (Grid cs) where
  type Rep (Grid cs) = Coord cs
  tabulate func = Grid $ V.fromList $ map func $ allCoord
  index (Grid v) c = v V.! coordPosition c

instance (All IsCoord cs) => FunctorWithIndex (Coord cs) (Grid cs) where
  imap func (Grid v) = Grid $ V.zipWith func (V.fromList allCoord) v

instance (All IsCoord cs) => FoldableWithIndex (Coord cs) (Grid cs) where
  ifoldMap func (Grid v) = foldMap id $ V.zipWith func (V.fromList allCoord) v

instance (All IsCoord cs) => TraversableWithIndex (Coord cs) (Grid cs) where
  itraverse func (Grid v) =
    Grid <$> sequenceA (V.zipWith func (V.fromList allCoord) v)

-- | The first element of a type level list
type family Head xs where
  Head (x ': xs) = x

-- | All but the first elements of a type level list
type family Tail xs where
  Tail (x ': xs) = xs

-- | Given a grid type, give back a series of nested lists repesenting the grid. The lists will have a number of layers equal to the dimensionality.
type family CollapseGrid cs a where
  CollapseGrid '[] a = a
  CollapseGrid (c ': cs) a = [CollapseGrid cs a]

-- | A Constraint that all grid sizes are instances of `KnownNat`
type family AllGridSizeKnown cs :: Constraint where
  AllGridSizeKnown '[] = ()
  AllGridSizeKnown cs = ( GHC.KnownNat (CoordSized (Head cs))
                        , GHC.KnownNat (MaxCoordSize (Tail cs))
                        , AllGridSizeKnown (Tail cs))

-- | Convert a vector into a list of `Vector`s, where all the elements of the list have the given size.
splitVectorBySize :: Int -> V.Vector a -> [V.Vector a]
splitVectorBySize n v
  | V.length v >= n = V.take n v : splitVectorBySize n (V.drop n v)
  | V.null v = []
  | otherwise = [v]

-- | Convert a grid to a series of nested lists. This removes type level information, but it is sometimes easier to work with lists
collapseGrid ::
     forall cs a. (SListI cs, AllGridSizeKnown cs)
  => Grid cs a
  -> CollapseGrid cs a
collapseGrid (Grid v) =
  case (shape :: Shape cs) of
    ShapeNil -> v V.! 0
    ShapeCons _ ->
      map (collapseGrid . Grid @(Tail cs)) $
      splitVectorBySize
        (fromIntegral $ GHC.natVal (Proxy @(MaxCoordSize (Tail cs))))
        v

-- | Convert a series of nested lists to a grid. If the size of the grid does not match the size of lists this will be `Nothing`
gridFromList ::
     forall cs a. (SListI cs, AllGridSizeKnown cs)
  => CollapseGrid cs a
  -> Maybe (Grid cs a)
gridFromList cg =
  case (shape :: Shape cs) of
    ShapeNil -> Just $ Grid $ V.singleton $ cg
    ShapeCons _ ->
      if length cg == fromIntegral (GHC.natVal (Proxy @(CoordSized (Head cs))))
        then Grid . mconcat <$>
             traverse (fmap unGrid . gridFromList @(Tail cs)) cg
        else Nothing

instance (AllGridSizeKnown cs, ToJSON a, SListI cs) => ToJSON (Grid cs a) where
  toJSON (Grid v) =
    case (shape :: Shape cs) of
      ShapeNil -> toJSON (v V.! 0)
      ShapeCons _ ->
        toJSON $
        map (toJSON . Grid @(Tail cs)) $
        splitVectorBySize
          (fromIntegral $ GHC.natVal (Proxy @(MaxCoordSize (Tail cs))))
          v

instance (All IsCoord cs, FromJSON a) => FromJSON (Grid cs a) where
  parseJSON v =
    case (shape :: Shape cs) of
      ShapeNil -> Grid . V.singleton <$> parseJSON v
      ShapeCons _ -> do
        a :: [Grid (Tail cs) a] <- parseJSON v
        return $ Grid $ foldMap unGrid a

transposeGrid ::
     ( IsCoord h
     , IsCoord w
     , GHC.KnownNat (MaxCoordSize '[ w, h])
     , GHC.KnownNat (MaxCoordSize '[ h, w])
     )
  => Grid '[ w, h] a
  -> Grid '[ h, w] a
transposeGrid g = tabulate $ \i -> index g $ tranposeCoord i

splitGrid ::
       forall c cs a. (AllSizedKnown cs)
    => Grid (c ': cs) a
    -> Grid '[ c] (Grid cs a)
splitGrid (Grid v) =
    withDict
        (sizeProof @cs)
        (Grid $
         V.fromList $
         map
             Grid
             (splitVectorBySize
                  (fromIntegral $ GHC.natVal (Proxy :: Proxy (MaxCoordSize cs)))
                  v))

combineGrid :: Grid '[c] (Grid cs a) -> Grid (c ': cs) a
combineGrid (Grid v) = Grid (v >>= unGrid)

combineHigherDim ::
       ( CoordFromNat a ~ CoordFromNat b
       , c ~ CoordFromNat a ((GHC.+) (CoordSized a) (CoordSized b)))
    => Grid (a ': as) x
    -> Grid (b ': as) x
    -> Grid (c ': as) x
combineHigherDim (Grid v1) (Grid v2) = Grid (v1 <> v2)

dropGrid ::
       KnownNat n
    => Proxy n
    -> Grid '[ c] x
    -> Grid '[ CoordFromNat c (CoordSized c - n)] x
dropGrid p (Grid v) = Grid $ V.drop (fromIntegral $ natVal p) v

takeGrid :: KnownNat n => Proxy n -> Grid '[c] x -> Grid '[CoordFromNat c n] x
takeGrid p (Grid v) = Grid $ V.take (fromIntegral $ natVal p) v


splitHigherDim ::
       forall a b c as x.
       ( KnownNat (CoordSized b)
       , c ~ CoordFromNat a (CoordSized a - CoordSized b)
       , CoordSized b <= CoordSized a
       , AllSizedKnown as
       )
    => Grid (a ': as) x
    -> (Grid (b ': as) x, Grid (c ': as) x)
splitHigherDim (Grid v) =
    let (a, b) =
            withDict
                (sizeProof @as)
                (V.splitAt
                     (fromIntegral $
                      GHC.natVal (Proxy @(CoordSized b)) *
                      GHC.natVal (Proxy @(MaxCoordSize as)))
                     v)
     in (Grid a, Grid b)

mapLowerDim ::
       forall as bs x y c f. (AllSizedKnown as, Applicative f)
    => (Grid as x -> f (Grid bs y))
    -> Grid (c ': as) x
    -> f (Grid (c ': bs) y)
mapLowerDim f (Grid v) =
    withDict
        (sizeProof @as)
        (fmap (Grid . V.concat) $
         traverse (fmap unGrid . f . Grid) $
         splitVectorBySize
             (fromIntegral (GHC.natVal (Proxy @(MaxCoordSize as))))
             v)

class ShrinkableGrid (as :: [*]) (bs :: [*]) where
  shrinkGrid :: Coord as -> Grid as x -> Grid bs x

instance ShrinkableGrid '[] '[] where
  shrinkGrid _ (Grid v) = Grid v

instance ( KnownNat (CoordSized b)
         , AllSizedKnown as
         , IsCoord a
         , ShrinkableGrid as bs
         , b ~ CoordFromNat a (CoordSized b)
         ) =>
         ShrinkableGrid (a ': as) (b ': bs) where
    shrinkGrid (c :| cs) =
        combineGrid . fmap (shrinkGrid cs) . helper . splitGrid
      where
        helper :: Grid '[ a] x -> Grid '[ b] x
        helper g =
            asSizeProxy c $ \(pTake :: Proxy n) ->
                case coordFromNatCollapse @a @(CoordSized a - n) @(CoordSized b) of
                    Refl ->
                        takeGrid (Proxy :: Proxy (CoordSized b)) $
                        dropGrid pTake g
    shrinkGrid _ = error "Unreachable pattern in shrinkGrid"

