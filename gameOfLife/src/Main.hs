{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}

module Main where

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Monad.Random
import           Data.AffineSpace
import           Data.Functor.Rep
import           Data.Semigroup         (Semigroup (..))
import           Generics.SOP
import           SizedGrid.Coord
import           SizedGrid.Coord.Class
import           SizedGrid.Grid.Focused
import           SizedGrid.Grid.Grid

data TileState
    = Alive
    | Dead
    deriving (Eq, Show)

type Rule = TileState -> [TileState] -> TileState

gameOfLife :: Rule
gameOfLife here neigh =
    let aliveNeigh = length $ filter (== Alive) neigh
    in if | here == Alive && aliveNeigh `elem` [2,3] -> Alive
          | here == Dead && aliveNeigh == 3 -> Alive
          | otherwise -> Dead

applyRule ::
       ( All Monoid cs
       , All IsCoord cs
       , All Semigroup cs
       , AllDiffSame Integer cs
       , All AffineSpace cs
       )
    => Rule
    -> FocusedGrid cs TileState
    -> FocusedGrid cs TileState
applyRule rule =
    extend $ \fg ->
        rule (extract fg) $ map (\p -> peek p fg) $ moorePoints 1 $ pos fg

main :: IO ()
main = putStrLn "Hello, Haskell!"
