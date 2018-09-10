{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Main where

import           SizedGrid

import           Control.Lens
import           Data.Maybe


newtype Symbol = Symbol (Ordinal 9)
  deriving (Eq,Show,Ord,Enum,Bounded)

displaySymbol :: Maybe Symbol -> String
displaySymbol (Just (Symbol n)) = show $ 1 + ordinalToNum n
displaySymbol _                 = "_"

type Board = Grid '[Ordinal 9, Ordinal 9] (Maybe Symbol)

main :: IO ()
main = putStrLn "Hello, Haskell!"

exampleGrid :: Board
exampleGrid =
    (\x -> Symbol <$> numToOrdinal (x - 1)) <$>
    fromJust (gridFromList
        ([ [0, 0, 3, 0, 2, 0, 6, 0, 0]
         , [9, 0, 0, 3, 0, 5, 0, 0, 1]
         , [0, 0, 1, 8, 0, 6, 4, 0, 0]
         , [0, 0, 8, 1, 0, 2, 9, 0, 0]
         , [7, 0, 0, 0, 0, 0, 0, 0, 8]
         , [0, 0, 6, 7, 0, 8, 2, 0, 0]
         , [0, 0, 2, 6, 0, 9, 5, 0, 0]
         , [8, 0, 0, 2, 0, 3, 0, 0, 9]
         , [0, 0, 5, 0, 1, 0, 3, 0, 0]
         ]))

rows :: Board -> [Grid '[Ordinal 1, Ordinal 9] (Maybe Symbol)]
rows = gridWindows
