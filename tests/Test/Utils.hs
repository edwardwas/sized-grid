{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Utils where

import           SizedGrid.Coord.Class
import           SizedGrid.Ordinal

#if MIN_VERSION_base(4,11,0)
#else
import           Data.Semigroup
#endif
import           Control.Lens
import           Data.AdditiveGroup
import           Data.Aeson
import           Data.AffineSpace
import           Data.Functor.Classes
import           Data.Functor.Compose
import           Data.Proxy
import           GHC.TypeLits
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

eq1Laws ::
       forall f. (Eq1 f, Applicative f)
    => Proxy f
    -> TestTree
eq1Laws _ =
    let nilEq =
            assertEqual "Nil equal" True $ liftEq (==) (pure ()) (pure @f ())
    in testGroup "Eq1 Laws" [testCase "Nil Eq" nilEq]

aesonLaws ::
     forall a proxy. (Show a, Eq a, ToJSON a, FromJSON a, Arbitrary a)
  => proxy a
  -> TestTree
aesonLaws _ =
  let encodeDecode :: a -> Property
      encodeDecode a = Just a === decode (encode a)
  in testGroup "Aeson Laws" [testProperty "Encode decode" encodeDecode]

semigroupLaws ::
     forall a proxy. (Show a, Eq a, Semigroup a, Arbitrary a)
  => proxy a
  -> TestTree
semigroupLaws _ =
  let assoc :: a -> a -> a -> Property
      assoc a b c = a <> (b <> c) === (a <> b) <> c
  in testGroup "Semigroup Laws" [testProperty "Associative" assoc]

monoidLaws ::
     forall a proxy. (Show a, Eq a, Monoid a, Arbitrary a)
  => proxy a
  -> TestTree
monoidLaws _ =
  let assoc :: a -> a -> a -> Property
      assoc a b c = mappend a (mappend b c) === mappend (mappend a b) c
      memptyId :: a -> Property
      memptyId a = (a === (mappend mempty a)) .&&. ((a === mappend a mempty))
      concatIsFold :: [a] -> Property
      concatIsFold as = mconcat as === foldr mappend mempty as
  in testGroup
       "Monoid laws"
       [ testProperty "Associative" assoc
       , testProperty "Mempty Id" memptyId
       , testProperty "Concat is Fold" concatIsFold
       ]

additiveGroupLaws ::
     forall a proxy. (Show a, Eq a, AdditiveGroup a, Arbitrary a)
  => proxy a
  -> TestTree
additiveGroupLaws _ =
  let assoc :: a -> a -> a -> Property
      assoc a b c = a ^+^ (b ^+^ c) === (a ^+^ b) ^+^ c
      zeroId :: a -> Property
      zeroId a = (a === zeroV ^+^ a) .&&. (a === a ^+^ zeroV)
      inverseId :: a -> Property
      inverseId a = a ^-^ a === zeroV
      takeLeaves :: a -> a -> Property
      takeLeaves a b = a ^-^ (a ^-^ b) === b
  in testGroup
       "AdditiveGroup laws"
       [ testProperty "Associative" assoc
       , testProperty "Zero Id" zeroId
       , testProperty "Inverse id is zeroV" inverseId
       , testProperty "a - (a - b) = b" takeLeaves
       ]

affineSpaceLaws ::
     forall a proxy.
     (Arbitrary a, Show a, Eq a, AffineSpace a, Eq (Diff a), Show (Diff a))
  => proxy a
  -> TestTree
affineSpaceLaws _ =
  let addZero :: a -> Property
      addZero a = a === a .+^ zeroV
      takeSelf :: a -> Property
      takeSelf a = a .-. a === zeroV
  in testGroup
       "AffineSpace Laws"
       [testProperty "Add Zero" addZero, testProperty "Take self" takeSelf]

applicativeLaws ::
     forall f a.
     ( Applicative f
     , Traversable f
     , Show (f a)
     , Eq (f a)
     , Show a
     , Arbitrary a
     , Arbitrary1 f
     , Function a
     , CoArbitrary a
     )
  => Proxy f
  -> Proxy a
  -> TestTree
applicativeLaws _ _ =
  let identiy :: Gen Property
      identiy = do
        v :: f a <- liftArbitrary arbitrary
        return (v === (pure id <*> v))
      homomorphism :: Gen Property
      homomorphism = do
        x :: a <- arbitrary
        f :: (a -> a) <- applyFun <$> arbitrary
        return ((pure f <*> pure x) === pure @f (f x))
  in testGroup
       "Applicative Laws"
       [ testProperty "Identity" (property identiy)
       , testProperty "Homomorphism" (property homomorphism)
       ]

traversalLaws ::
     forall a f b.
     ( Eq a
     , Show a
     , Functor f
     , Arbitrary a
     , Function b
     , CoArbitrary b
     , Arbitrary b
     )
  => Traversal' a (f b)
  -> TestTree
traversalLaws t =
  let pureId =
        property $ do
          a :: a <- arbitrary
          return (pure @[] a === t pure a)
      compose =
        property $ do
          a :: a <- arbitrary
          fFunc :: b -> b <- applyFun <$> arbitrary
          gFunc :: b -> b <- applyFun <$> arbitrary
          let raiseFunc f x = Just (f <$> x)
          return
            (fmap (t (raiseFunc fFunc)) (t (raiseFunc gFunc) a) ===
             getCompose
               (t (Compose . fmap (raiseFunc fFunc) . (raiseFunc gFunc)) a))
  in testGroup
       "Traveral Laws"
       [testProperty "Pure Id" pureId, testProperty "Compose" compose]

isCoordLaws ::
     forall a. (IsCoord a)
  => Proxy a
  -> TestTree
isCoordLaws p =
  testCase "IsCoord Laws" $ do
    assertEqual
      "Max coord size is sCoordSized"
      (maxCoordSize p)
      (natVal (sCoordSized p) - 1)
    assertEqual
      "zeroPosition is Zero"
      (0 :: Int)
      (ordinalToNum $ view asOrdinal (zeroPosition @a))
    assertEqual
      "Size Proxy Zero"
      (0 :: Integer)
      (asSizeProxy (zeroPosition @a) natVal)
    assertEqual
      "Max size equality"
      (ordinalToNum $ view asOrdinal (maxCoord @a))
      (maxCoordSize p)

