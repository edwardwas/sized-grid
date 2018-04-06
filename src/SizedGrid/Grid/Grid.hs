{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module SizedGrid.Grid.Grid where

import           SizedGrid.Coord
import           SizedGrid.Coord.Class

import           Control.Lens          hiding (index)
import           Data.Aeson
import           Data.Distributive
import           Data.Functor.Rep
import           Data.Proxy            (Proxy (..))
import qualified Data.Vector           as V
import           Generics.SOP
import           GHC.Exts
import qualified GHC.TypeLits          as GHC

data Grid (cs :: [*]) a = Grid
    { unGrid :: V.Vector a
    } deriving (Eq, Show, Functor, Foldable, Traversable)

instance GHC.KnownNat (MaxCoordSize cs) => Applicative (Grid cs) where
    pure =
        Grid .
        V.replicate
            (fromIntegral $ GHC.natVal (Proxy :: Proxy (MaxCoordSize cs)))
    Grid fs <*> Grid as = Grid $ V.zipWith ($) fs as

instance (GHC.KnownNat (MaxCoordSize cs), All IsCoord cs) => Monad (Grid cs) where
  g >>= f = imap (\p a -> f a `index` p) g

instance (GHC.KnownNat (MaxCoordSize cs), All IsCoord cs) =>
         Distributive (Grid cs) where
    distribute = distributeRep

instance (All IsCoord cs, GHC.KnownNat (MaxCoordSize cs)) =>
         Representable (Grid cs) where
    type Rep (Grid cs) = Coord cs
    tabulate func = Grid $ V.fromList $ map func $ allCoord
    index (Grid v) c = v V.! coordPosition c

instance (All IsCoord cs) =>
         FunctorWithIndex (Coord cs) (Grid cs) where
    imap func (Grid v) = Grid $ V.zipWith func (V.fromList allCoord) v

instance (All IsCoord cs) =>
         FoldableWithIndex (Coord cs) (Grid cs) where
    ifoldMap func (Grid v) = foldMap id $ V.zipWith func (V.fromList allCoord) v

instance (All IsCoord cs) =>
         TraversableWithIndex (Coord cs) (Grid cs) where
    itraverse func (Grid v) =
        Grid <$> sequenceA (V.zipWith func (V.fromList allCoord) v)

type family Head xs where
  Head (x ': xs) = x

type family Tail xs where
  Tail (x ': xs) = xs

type family CollapseGrid cs a where
  CollapseGrid '[] a = a
  CollapseGrid (c ': cs) a = [CollapseGrid cs a]

type family AllGridSizeKnown cs :: Constraint where
  AllGridSizeKnown '[] = ()
  AllGridSizeKnown cs = (GHC.KnownNat (MaxCoordSize (Tail cs)), AllGridSizeKnown (Tail cs))

splitVectorBySize :: Int -> V.Vector a -> [V.Vector a]
splitVectorBySize n v
  | V.length v >= n = V.take n v : splitVectorBySize n (V.drop n v)
  | V.null v = []
  | otherwise = [v]

collapseGrid ::
       forall cs a.
       ( SListI cs
       , AllGridSizeKnown cs
       )
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

gridFromList ::
     forall cs a. SListI cs
  => CollapseGrid cs a
  -> Maybe (Grid cs a)
gridFromList cg =
  case (shape :: Shape cs) of
    ShapeNil    -> Just $ Grid $ V.singleton $ cg
    ShapeCons _ -> Grid . mconcat <$> traverse (fmap unGrid . gridFromList @(Tail cs)) cg

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
  parseJSON v = case (shape :: Shape cs) of
    ShapeNil -> Grid . V.singleton <$> parseJSON v
    ShapeCons _ -> do
      a :: [Grid (Tail cs) a] <- parseJSON v
      return $ Grid $ foldMap unGrid a
