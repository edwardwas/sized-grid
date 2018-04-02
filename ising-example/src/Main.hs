{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           SizedGrid.Coord
import           SizedGrid.Coord.Periodic
import           SizedGrid.Grid.Class
import           SizedGrid.Grid.Focused
import           SizedGrid.Grid.Grid

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Control.Monad.State
import           Data.AdditiveGroup
import           Generics.SOP
import           System.Random

data Spin = Up | Down deriving (Eq,Show,Enum,Bounded)

flipSpin Up   = Down
flipSpin Down = Up

spinNumber :: Num a => Spin -> a
spinNumber Up   = 1
spinNumber Down = -1

instance Random Spin where
  random g =
    let (a, g') = random g
    in if a
         then (Up, g')
         else (Down, g')
  randomR (mi, ma) g =
    let toBool Up   = True
        toBool Down = False
        (a,g') = randomR (toBool mi, toBool ma) g
    in if a
         then (Up, g')
         else (Down, g')

type GridType = '[Periodic 10, Periodic 10]

randomGrid :: RandomGen g => g -> (Grid GridType Spin, g)
randomGrid g = runState (sequenceA $ pure $ state random) g

randomGrids :: RandomGen g => g -> [(Grid GridType Spin, g)]
randomGrids startG = tail $ iterate (\(_,g) -> randomGrid g) (undefined,startG)

data PhysicalOptions = PhysicalOptions
  { coupling       :: Double
  , magneticMoment :: Double
  } deriving (Eq, Show)

singleEnergy :: PhysicalOptions -> FocusedGrid GridType Spin -> Double
singleEnergy PhysicalOptions {..} fg =
    let neigh = filter (/= (pos fg)) $ vonNeumanPoints 1 (pos fg)
    in (-magneticMoment) * (spinNumber $ extract fg) -
       sum
           (map (\p ->
                     coupling * spinNumber (extract fg) * spinNumber (peek p fg))
                neigh)

totalEnergy :: PhysicalOptions -> FocusedGrid GridType Spin -> Double
totalEnergy po = sum . extend (singleEnergy po)

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

startEnergyMean po n =
  mean . map (totalEnergy po . view asFocusedGrid . fst) . take n . randomGrids

performFlip ::
       RandomGen g
    => PhysicalOptions
    -> FocusedGrid GridType Spin
    -> Coord GridType
    -> g
    -> (FocusedGrid GridType Spin, g)
performFlip po fg pos g =
    let oldEnergy = singleEnergy po $ seek pos fg
        newGrid = fg & gridIndex pos %~ flipSpin
        newEnergy = singleEnergy po $ seek pos newGrid
        acceptanceProb = exp (oldEnergy - newEnergy)
        (a :: Double, g') = random g
    in if newEnergy > oldEnergy && a <= acceptanceProb
           then (newGrid, g')
           else (fg, g')

doSimulation ::
       (MonadState g m, RandomGen g)
    => PhysicalOptions
    -> Int
    -> FocusedGrid GridType Spin
    -> m (FocusedGrid GridType Spin)
doSimulation po n startGrid = do
    g <- get
    let (g1, g2) = split g
        ps :: [Coord GridType] = take n $ randoms g1
    put g2
    foldM (\p fg -> state (performFlip po p fg)) startGrid ps


main :: IO ()
main = do
    putStrLn "Lets go"
    g <- newStdGen
    let po = PhysicalOptions 1 0
        (startGrid, g') = randomGrid g
        (endGrid, g'') =
            over _1 (view asGrid) $
            runState (doSimulation po (floor $ 1e4) $ view asFocusedGrid startGrid) g'
    putStrLn $
        "Start energy: " ++ show (totalEnergy po $ startGrid ^. asFocusedGrid)
    putStrLn $
        "End energy: " ++ show (totalEnergy po $ endGrid ^. asFocusedGrid)
