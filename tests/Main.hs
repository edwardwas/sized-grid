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
import           SizedGrid.Ordinal
import           SizedGrid.Peano
import           SizedGrid.Type.Number

import           Test.Utils

import           Control.Lens
import           Generics.SOP             hiding (S, Z)
import           Hedgehog
import qualified Hedgehog.Gen             as Gen
import           Test.Tasty

genPeriodic :: (n ~ S x, SPeanoI x) => Gen (Periodic n)
genPeriodic = Periodic <$> Gen.enumBounded

genCoord :: SListI cs => NP Gen cs -> Gen (Coord cs)
genCoord start = Coord <$> hsequence start

main :: IO ()
main =
    let periodic =
            let g :: Gen (Periodic (AsPeano 10)) = genPeriodic
            in [ semigroupLaws g
               , monoidLaws g
               , additiveGroupLaws g
               , affineSpaceLaws g
               ]
        hardWrap =
            let g :: Gen (HardWrap (AsPeano 10)) = HardWrap <$> Gen.enumBounded
            in [semigroupLaws g, monoidLaws g, affineSpaceLaws g]
        coord =
            let g :: Gen (Coord '[ HardWrap (AsPeano 10), Periodic (AsPeano 20)]) =
                    genCoord
                        ((HardWrap <$> Gen.enumBounded) :*
                         (Periodic <$> Gen.enumBounded) :*
                         Nil)
            in [semigroupLaws g, monoidLaws g, affineSpaceLaws g]
        coord2 =
            let g :: Gen (Coord '[ Periodic (AsPeano 10), Periodic (AsPeano 20)]) =
                    genCoord
                        ((Periodic <$> Gen.enumBounded) :*
                         (Periodic <$> Gen.enumBounded) :*
                         Nil)
            in [semigroupLaws g, monoidLaws g, affineSpaceLaws g, additiveGroupLaws g]
    in defaultMain $
       testGroup
           "tests"
           [ testGroup "Periodic 20" periodic
           , testGroup "HardWrap 20" hardWrap
           , testGroup "Coord [HardWrap 10, Periodic 20]" coord
           , testGroup "Coord [Periodic 10, Periodic 20]" coord2
           ]
