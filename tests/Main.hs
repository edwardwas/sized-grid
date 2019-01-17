{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           SizedGrid

import           Test.Shrink
import           Test.Utils

import           Control.Lens          hiding (index)
import           Control.Monad         (replicateM)
import           Data.Functor.Rep
import           Data.Proxy
import qualified Data.Vector           as V
import           Generics.SOP          hiding (S, Z)
import qualified GHC.Generics          as GHC
import           GHC.TypeLits
import qualified GHC.TypeLits          as GHC
import           Test.QuickCheck       (Arbitrary (..), Arbitrary1 (..),
                                        Property, oneof, property, (.&&.),
                                        (===))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

instance (1 <= n, KnownNat n) => Arbitrary (Periodic n) where
  arbitrary = Periodic <$> oneof (map pure [minBound .. maxBound])

instance (1 <= n, KnownNat n) => Arbitrary (HardWrap n) where
  arbitrary = HardWrap <$> oneof (map pure [minBound .. maxBound])

instance (All Arbitrary cs, SListI cs) => Arbitrary (Coord cs) where
  arbitrary = Coord <$> hsequence (hcpure (Proxy @Arbitrary) arbitrary)

instance AllSizedKnown cs => Arbitrary1 (Grid cs) where
  liftArbitrary g = sequenceA  (pure g)

instance (AllSizedKnown cs, Arbitrary a) => Arbitrary (Grid cs a) where
  arbitrary = liftArbitrary arbitrary

assertOrderd :: Ord a => [a] -> Assertion
assertOrderd =
    let helper []     = True
        helper (x:xs) = all (x <=) xs && helper xs
     in assertBool "Ordered" . helper

testAllCoordOrdered ::
       forall cs proxy. (All Eq cs, All Ord cs, All IsCoordLifted cs)
    => proxy (Coord cs)
    -> TestTree
testAllCoordOrdered _ =
    testCase "allCoord is ordered" $ assertOrderd (allCoord @cs)

gridTests ::
       forall cs a x y f g.
       ( Show (Coord cs)
       , Eq (Coord cs)
       , All IsCoordLifted cs
       , AllSizedKnown cs
       , Show a
       , Eq a
       , cs ~ '[ f x, g y]
       , KnownNat (y GHC.* x)
       , KnownNat (x GHC.* y)
       , Arbitrary a
       , Arbitrary (f x)
       , Arbitrary (g y)
       )
    => Proxy (Coord cs)
    -> Proxy a
    -> [TestTree]
gridTests genC genA =
  let tabulateIndex :: Coord cs -> Property
      tabulateIndex c = c === index (tabulate id :: Grid cs (Coord cs)) c
      collapseUnCollapse :: Property
      collapseUnCollapse =
        property $ do
          g :: Grid cs a <- sequenceA $ pure arbitrary
          return (Just g === gridFromList (collapseGrid g))
      uncollapseCollapse =
        property $ do
          cg :: [[a]] <-
            replicateM (fromIntegral $ natVal (Proxy @x)) $
            replicateM (fromIntegral $ natVal (Proxy @y)) $
            arbitrary
          return (Just cg === (collapseGrid <$> gridFromList @cs cg))
      doubleTranspose =
        property $ do
          g :: Grid cs a <- sequenceA $ pure arbitrary
          return (g === transposeGrid (transposeGrid g))
  in [ testProperty "Tabulate index" tabulateIndex
     , testProperty "Collapse UnCollapse" collapseUnCollapse
     , testProperty "UnCollapse and Collapse" uncollapseCollapse
     , testProperty "Transpose twice is id" doubleTranspose
     ]

splitTests ::
       forall c x cs a.
       ( Show a
       , Eq a
       , Num a
       , All IsCoordLifted ((c x) ': cs)
       , KnownNat (x GHC.* MaxCoordSize cs)
       , KnownNat (MaxCoordSize cs)
       , KnownNat (5 GHC.* MaxCoordSize cs)
       , KnownNat (3 GHC.* MaxCoordSize cs)
       , KnownNat (2 GHC.* MaxCoordSize cs)
       , KnownNat (2 GHC.* MaxCoordSize cs)
       , KnownNat 2
       , AllSizedKnown cs
       , Arbitrary a
       )
    => Proxy ((c x) ': cs)
    -> Proxy a
    -> [TestTree]
splitTests _ _ =
  let splitAndCombine =
        property $ do
          g :: Grid ((c x) ': cs) a <- sequenceA $ pure arbitrary
          return (g === combineGrid (splitGrid g))
      combineAndSplit =
        property $ do
          g :: Grid '[ c x] (Grid cs a) <-
            sequenceA $ pure (sequenceA $ pure arbitrary)
          return (g === splitGrid (combineGrid g))
      higherSplitAndCombine =
        property $ do
          g :: Grid (Ordinal 5 ': cs) a <- sequenceA $ pure arbitrary
          let (a :: Grid (Ordinal 3 ': cs) a, b) = splitHigherDim g
          return (g === combineHigherDim a b)
      higherCombineAndSplit =
        property $ do
          g1 :: Grid (Ordinal 3 ': cs) a <- sequenceA $ pure arbitrary
          g2 :: Grid (Ordinal 2 ': cs) a <- sequenceA $ pure arbitrary
          let g = combineHigherDim g1 g2
          return ((g1, g2) === splitHigherDim g)
  in [ testProperty "Split and Combine" splitAndCombine
     , testProperty "Combine and split" combineAndSplit
     , testProperty "Split and Combine Higher dim" higherSplitAndCombine
     , testProperty "Combine and Split Higher dim" higherCombineAndSplit
     ]

twoDimensionalCoordTests ::
     forall cs x y . (cs ~ '[ x, y], All Show cs, All Eq cs, All Arbitrary cs)
  => Proxy (Coord cs)
  -> [TestTree]
twoDimensionalCoordTests _ =
  let doubleTranspose :: Coord cs -> Property
      doubleTranspose c = c === tranposeCoord (tranposeCoord c)
  in [testProperty "Transpose twice is id" doubleTranspose]

coordCreationTests ::
     forall cs a c.
     ( All Show cs
     , All Eq cs
     , Eq a
     , Show a
     , Show c
     , Eq c
     , Arbitrary a
     , All Arbitrary cs
     , Arbitrary c
     )
  => Proxy (Coord (c ': cs))
  -> Proxy a
  -> [TestTree]
coordCreationTests genC gen =
  [ testProperty "Create single coord" $
    property $ \(g :: a) -> g === (singleCoord g ^. _1)
  , testProperty "Create double coord" $
    property $ \(a :: a) (b :: a) ->
      let coord = b :| singleCoord a
      in (a === coord ^. _2) .&&. (b === coord ^. _1)
  , testProperty "Create triple coord" $
    property $ \(a :: a) (b :: a) (c :: a) ->
      let coord = c :| (b :| singleCoord a)
      in (a === coord ^. _3) .&&. (b === coord ^. _2) .&&. (c === coord ^. _1)
  , testProperty "Head and append" $
    property $ \(coord :: Coord (c ': cs)) (a :: a) ->
      let newCoord = appendCoord a coord
      in (a === newCoord ^. coordHead) .&&. (coord === newCoord ^. coordTail)
  , testProperty "Tail destruction" $
    property $ \(coord :: Coord (c ': cs)) ->
      appendCoord (coord ^. coordHead) (coord ^. coordTail) === coord
  ]

main :: IO ()
main =
  let periodic =
        let p = Proxy @(Periodic 10)
        in [ semigroupLaws p
           , monoidLaws p
           , additiveGroupLaws p
           , affineSpaceLaws p
           , aesonLaws p
           , isCoordLaws p
           ]
      hardWrap =
        let p = Proxy @(HardWrap 10)
        in [ semigroupLaws p
           , monoidLaws p
           , affineSpaceLaws p
           , aesonLaws p
           , isCoordLaws p
           ]
      coord =
        let p = Proxy @(Coord '[ HardWrap 10, Periodic 20])
        in [ semigroupLaws p
           , monoidLaws p
           , affineSpaceLaws p
           , aesonLaws p
           , testAllCoordOrdered p
           ]
      coord2 =
        let p = Proxy @(Coord '[ Periodic 10, Periodic 20])
        in [ semigroupLaws p
           , monoidLaws p
           , affineSpaceLaws p
           , additiveGroupLaws p
           , aesonLaws p
           , testAllCoordOrdered p
           ]
  in defaultMain $
     testGroup
       "tests"
       [ testGroup "Periodic 20" periodic
       , testGroup "HardWrap 20" hardWrap
       , testGroup "Coord [HardWrap 10, Periodic 20]" coord
       , testGroup "Coord [Periodic 10, Periodic 20]" coord2
       , testGroup "2D Coords" $
         twoDimensionalCoordTests (Proxy @(Coord '[ HardWrap 10, Periodic 10]))
       , testGroup
           "Coord creation"
           (coordCreationTests
              (Proxy @(Coord '[ HardWrap 10, Periodic 10]))
              (Proxy @Int))
       , testGroup
           "Grid"
           ((gridTests
               (Proxy @(Coord ('[ Periodic 10, Periodic 11])))
               (Proxy @Int) ++
             [ applicativeLaws
                 (Proxy @(Grid '[ Periodic 10, Periodic 11]))
                 (Proxy @Int)
             , aesonLaws (Proxy @(Grid '[ Periodic 10, Periodic 11] Int))
             , eq1Laws (Proxy @(Grid '[ Periodic 10, Periodic 20]))
             ]))
       , testGroup
           "Splitting"
           (splitTests
              (Proxy @('[ HardWrap 8, HardWrap 3, HardWrap 5]))
              (Proxy @Int))
       , shrinkTests
       ]
