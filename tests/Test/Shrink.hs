{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Shrink where

import           Data.Maybe       (fromJust)
import           SizedGrid
import           Test.Tasty
import           Test.Tasty.HUnit

exampleGrid :: Grid '[Ordinal 3, Ordinal 3] Int
exampleGrid = fromJust $ gridFromList [[1,2,3],[4,5,6],[7,8,9]]

focusCenter :: Grid '[ Ordinal 1, Ordinal 1] Int
focusCenter =
    let c :: Coord '[Ordinal 3, Ordinal 3] =
            fromJust $
            (\x y -> x :| y :| EmptyCoord) <$> numToOrdinal (1 :: Int) <*>
            numToOrdinal (1 :: Int)
     in shrinkGrid c exampleGrid

shrinkTests :: TestTree
shrinkTests = testCase "Focusing" $ do
  assertEqual "Focus Center" focusCenter $ fromJust (gridFromList [[5]])
