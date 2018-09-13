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
import           Hedgehog
import qualified Hedgehog.Gen          as Gen
import qualified Hedgehog.Range        as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit

eq1Laws ::
       forall f. (Eq1 f, Applicative f)
    => Proxy f
    -> TestTree
eq1Laws _ =
    let nilEq =
            assertEqual "Nil equal" True $ liftEq (==) (pure ()) (pure @f ())
    in testGroup "Eq1 Laws" [testCase "Nil Eq" nilEq]

aesonLaws :: (Show a, Eq a, ToJSON a, FromJSON a) => Gen a -> TestTree
aesonLaws gen =
    let encodeDecode = property $ do
          a <- forAll gen
          Just a === decode (encode a)
    in testGroup "Aeson Laws" [testProperty "Encode decode" encodeDecode]

semigroupLaws :: (Show a, Eq a, Semigroup a) => Gen a -> TestTree
semigroupLaws gen =
  let assoc = property $ do
         a <- forAll gen
         b <- forAll gen
         c <- forAll gen
         a <> (b <> c) === (a <> b) <> c
  in testGroup "Semigroup Laws" [testProperty "Associative" assoc]

monoidLaws :: (Show a, Eq a, Monoid a) => Gen a -> TestTree
monoidLaws gen =
  let assoc =
        property $ do
          a <- forAll gen
          b <- forAll gen
          c <- forAll gen
          mappend a (mappend b c) === mappend (mappend a b) c
      memptyId =
        property $ do
          a <- forAll gen
          a === mappend mempty a
          a === mappend a mempty
      concatIsFold =
        property $ do
          as <- forAll $ Gen.list (Range.linear 0 100) gen
          mconcat as === foldr mappend mempty as
  in testGroup
       "Monoid laws"
       [ testProperty "Associative" assoc
       , testProperty "Mempty Id" memptyId
       , testProperty "Concat is Fold" concatIsFold
       ]

additiveGroupLaws :: (Show a, Eq a, AdditiveGroup a) => Gen a -> TestTree
additiveGroupLaws gen =
  let assoc =
        property $ do
          a <- forAll gen
          b <- forAll gen
          c <- forAll gen
          a ^+^  (b ^+^ c) === (a ^+^  b) ^+^ c
      zeroId =
        property $ do
          a <- forAll gen
          a === zeroV ^+^ a
          a === a ^+^ zeroV
      inverseId = property $ do
          a <- forAll gen
          a ^-^ a === zeroV
      takeLeaves = property $ do
          a <- forAll gen
          b <- forAll gen
          a ^-^ (a ^-^ b) === b
  in testGroup
       "AdditiveGroup laws"
       [ testProperty "Associative" assoc
       , testProperty "Zero Id" zeroId
       , testProperty "Inverse id is zeroV" inverseId
       , testProperty "a - (a - b) = b" takeLeaves
       ]

affineSpaceLaws ::
       (Show a, Eq a, AffineSpace a, Eq (Diff a), Show (Diff a))
    => Gen a
    -> TestTree
affineSpaceLaws gen =
    let addZero =
            property $ do
                a <- forAll gen
                a === a .+^ zeroV
        takeSelf =
            property $ do
                a <- forAll gen
                a .-. a === zeroV
    in testGroup
           "AffineSpace Laws"
           [testProperty "Add Zero" addZero, testProperty "Take self" takeSelf]

applicativeLaws ::
       forall f a.
       (Applicative f, Traversable f, Show (f a), Eq (f a), Num a, Show a)
    => Proxy f
    -> Gen a
    -> TestTree
applicativeLaws _ gen =
    let genF :: Gen (f a) = sequence $ pure gen
        identiy =
            property $ do
                v <- forAll genF
                v === (pure id <*> v)
        homomorphism =
            property $ do
                x <- forAll gen
                f <- (+) <$> forAll gen
                (pure f <*> pure x) === pure @f (f x)
    in testGroup
           "Applicative Laws"
           [ testProperty "Identity" identiy
           , testProperty "Homomorphism" homomorphism
           ]

traversalLaws ::
       (Eq a, Show a, Num b, Functor f)
    => Gen a
    -> Traversal' a (f b)
    -> TestTree
traversalLaws g t =
    let pureId =
            property $ do
                a <- forAll g
                pure @[] a === t pure a
        compose =
            property $ do
                a <- forAll g
                let fFunc = \x -> Just ((* 3) <$> x)
                let gFunc = \y -> Just ((+ 2) <$> y)
                fmap (t fFunc) (t gFunc a) ===
                    getCompose (t (Compose . fmap fFunc . gFunc) a)
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
        assertEqual "Size Proxy Zero"
            (0 :: Integer) (asSizeProxy (zeroPosition @a) natVal)
        assertEqual "Max size equality" (ordinalToNum $ view asOrdinal (maxCoord @ a)) (maxCoordSize p)

