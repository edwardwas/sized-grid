{-# LANGUAGE DataKinds #-}

module Test.Shrink where

import           Data.Maybe (fromJust)
import           Data.Proxy
import           SizedGrid

exampleGrid :: Grid '[Ordinal 3, Ordinal 3] Int
exampleGrid = fromJust $ gridFromList [[1,2,3],[4,5,6],[7,8,9]]
