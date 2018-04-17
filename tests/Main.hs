{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           SizedGrid.Coord
import           SizedGrid.Coord.Class
import           SizedGrid.Coord.HardWrap
import           SizedGrid.Coord.Periodic
import           SizedGrid.Grid.Grid

import           Test.Utils

import           Control.Monad            (replicateM)
import           Data.Functor.Rep
import           Data.Proxy
import           Generics.SOP             hiding (S, Z)
import           GHC.TypeLits
import qualified GHC.TypeLits             as GHC
import           Hedgehog
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit

assertOrderd :: Ord a => [a] -> Assertion
assertOrderd =
    let helper []     = True
        helper (x:xs) = all (x <=) xs && helper xs
    in assertBool "Ordered" . helper

testAllCoordOrdered ::
       forall cs proxy. (All Eq cs, All Ord cs, All IsCoord cs)
    => proxy (Coord cs)
    -> TestTree
testAllCoordOrdered _ =
    testCase "allCoord is ordered" $ assertOrderd (allCoord @cs)

genPeriodic :: (1 <= n, GHC.KnownNat n) => Gen (Periodic n)
genPeriodic = Periodic <$> Gen.enumBounded

genCoord :: SListI cs => NP Gen cs -> Gen (Coord cs)
genCoord start = Coord <$> hsequence start

gridTests ::
       forall cs a x y.
       ( Show (Coord cs)
       , Eq (Coord cs)
       , All IsCoord cs
       , GHC.KnownNat (MaxCoordSize cs)
       , Show a
       , Eq a
       , AllGridSizeKnown cs
       , cs ~ '[x,y]
       )
    => Gen (Coord cs)
    -> Gen a
    -> [TestTree]
gridTests genC genA =
    let tabulateIndex =
            property $ do
                c <- forAll genC
                c === index (tabulate id :: Grid cs (Coord cs)) c
        collapseUnCollapse =
            property $ do
                g :: Grid cs a <- forAll (sequenceA $ pure genA)
                Just g === gridFromList (collapseGrid g)
        uncollapseCollapse =
            property $ do
                cg :: [[a]] <-
                    replicateM (fromIntegral $ natVal (Proxy @(CoordSized x))) $
                    replicateM (fromIntegral $ natVal (Proxy @(CoordSized y))) $ forAll genA
                Just cg === (collapseGrid <$> gridFromList @cs cg)
    in [ testProperty "Tabulate index" tabulateIndex
       , testProperty "Collapse UnCollapse" collapseUnCollapse
       , testProperty "UnCollapse and Collapse" uncollapseCollapse
       ]

main :: IO ()
main =
    let periodic =
            let g :: Gen (Periodic 10) = genPeriodic
            in [ semigroupLaws g
               , monoidLaws g
               , additiveGroupLaws g
               , affineSpaceLaws g
               , aesonLaws g
               ]
        hardWrap =
            let g :: Gen (HardWrap 10) = HardWrap <$> Gen.enumBounded
            in [semigroupLaws g, monoidLaws g, affineSpaceLaws g, aesonLaws g]
        coord =
            let g :: Gen (Coord '[ HardWrap 10, Periodic 20]) =
                    genCoord
                        ((HardWrap <$> Gen.enumBounded) :*
                         (Periodic <$> Gen.enumBounded) :*
                         Nil)
            in [semigroupLaws g, monoidLaws g, affineSpaceLaws g, aesonLaws g, testAllCoordOrdered g]
        coord2 =
            let g :: Gen (Coord '[ Periodic 10, Periodic 20]) =
                    genCoord
                        ((Periodic <$> Gen.enumBounded) :*
                         (Periodic <$> Gen.enumBounded) :*
                         Nil)
            in [ semigroupLaws g
               , monoidLaws g
               , affineSpaceLaws g
               , additiveGroupLaws g
               , aesonLaws g
               , testAllCoordOrdered g
               ]
    in defaultMain $
       testGroup
           "tests"
           [ testGroup "Periodic 20" periodic
           , testGroup "HardWrap 20" hardWrap
           , testGroup "Coord [HardWrap 10, Periodic 20]" coord
           , testGroup "Coord [Periodic 10, Periodic 20]" coord2
           , testGroup
                 "Grid"
                 ((gridTests @'[ Periodic 10, Periodic 11]
                   (genCoord $
                   (Periodic <$> Gen.enumBounded) :*
                   (Periodic <$> Gen.enumBounded) :*
                   Nil)) (Gen.int $ Range.linear 0 100) ++
                  [ applicativeLaws
                        (Proxy @(Grid '[ Periodic 10, Periodic 11]))
                        (Gen.int $ Range.linear 0 100)
                  , aesonLaws (sequenceA $ pure @(Grid '[Periodic 10, Periodic 11] ) $
                      Gen.int $ Range.linear 0 100)
                  , eq1Laws (Proxy @(Grid '[Periodic 10, Periodic 20]))
                  ])
           ]
