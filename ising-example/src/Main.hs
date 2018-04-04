{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
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
import           Control.Monad
import           Control.Monad.Random
import           Data.AdditiveGroup
import           Data.AffineSpace
import           Data.Monoid                            ((<>))
import           Data.Proxy
import           Generics.SOP                           hiding (Proxy (..))
import qualified GHC.TypeLits                           as GHC
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Simulate
import           Pipes                                  hiding (Proxy (..),
                                                         each)
import qualified Pipes.Prelude                          as P
import           System.Random

data Spin = Up | Down deriving (Eq,Show,Enum,Bounded)

flipSpin Up   = Down
flipSpin Down = Up

spinNumber :: Num a => Spin -> a
spinNumber Up   = 1
spinNumber Down = -1

data PhysicalOptions = PhysicalOptions
  { coupling :: Double
  } deriving (Eq, Show)

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

type GridType = '[Periodic 60, Periodic 60]
--type GridType = '[Periodic (AsPeano 1), Periodic (AsPeano 1)]

gridSize = GHC.natVal (Proxy :: Proxy (MaxCoordSize GridType))

randomGrid ::
     (GHC.KnownNat (MaxCoordSize cs), MonadRandom m)
  => m (Grid cs Spin)
randomGrid = sequence $ pure $ getRandom

singleEnergy :: PhysicalOptions -> FocusedGrid GridType Spin -> Double
singleEnergy PhysicalOptions {..} fg =
  -0.5 * coupling *
  sum
    (map (\p -> spinNumber (peek p fg) * spinNumber (extract fg)) $
     filter (/= (pos fg)) $ vonNeumanPoints 1 (pos fg))

totalEnergy ::
     IsGrid GridType (grid GridType)
  => PhysicalOptions
  -> grid GridType Spin
  -> Double
totalEnergy po = sum . extend (singleEnergy po) . view asFocusedGrid

energyAtPoint ::
     IsGrid GridType (grid GridType)
  => PhysicalOptions
  -> grid GridType Spin
  -> Coord GridType
  -> Double
energyAtPoint po g pos = singleEnergy po $ seek pos $ g ^. asFocusedGrid

attempFlip ::
     (IsGrid GridType (grid GridType), MonadRandom m)
  => PhysicalOptions
  -> grid GridType Spin
  -> Coord GridType
  -> m (grid GridType Spin)
attempFlip po start pos = do
  let startEnergy = energyAtPoint po start pos
      newGrid = start & gridIndex pos %~ flipSpin
      newEnergy = energyAtPoint po newGrid pos
      acceptProb = min 1 $ exp (startEnergy - newEnergy)
  a :: Double <- getRandom
  return
    (if newEnergy >= startEnergy && a >= acceptProb
       then start
       else newGrid)

runSimulation ::
     forall m. MonadRandom m
  => PhysicalOptions
  -> Int
  -> Producer' (Grid GridType Spin) m ()
runSimulation po n =
  P.replicateM (n * fromIntegral gridSize) (getRandom :: m (Coord GridType)) >->
  P.scanM (attempFlip po) randomGrid pure >-> takeOneIn 100

takeOneIn :: Monad m => Int -> Pipe a a m ()
takeOneIn n = forever $ do
  a <- await
  _ <- replicateM (n - 1) await
  yield a

data SimulationState = SimulationState
    { _current              :: Grid GridType Spin
    , _stepPerTime          :: Float
    , _elapsedSinceLastStep :: Float
    , _gen                  :: StdGen
    } deriving (Show)
makeLenses ''SimulationState

displaySimulation po startSimulationState =
    let draw = ifoldMapOf (current . itraversed) drawHelper
        drawHelper p a =
            let c =
                    if a == Up
                        then red
                        else blue
                (x, y) = p .-. mempty
            in translate
                   (8 * fromIntegral x)
                   (8 * fromIntegral y)
                    $ color c (translate 1 1 $ rectangleSolid 8 8)
        update vp dt old
            | old ^. elapsedSinceLastStep + dt >= old ^. stepPerTime =
                let (Just newGrid, g') = runRand (P.last (runSimulation po 1)) (old ^. gen)
                in update vp (dt - old ^. stepPerTime) $
                   old & (current .~ newGrid) &
                   (elapsedSinceLastStep -~ old ^. stepPerTime) &
                   (gen .~ g')
            | otherwise = old & elapsedSinceLastStep +~ dt
    in simulate
           (InWindow "floatMe" (800, 800) (1, 1))
           white
           60
           startSimulationState
           (translate (-350) (-350) . draw)
           update

main =
    let po = PhysicalOptions 10
    in do g <- newStdGen
          startGrid <- randomGrid
          displaySimulation po $
              SimulationState
              { _current = startGrid
              , _stepPerTime =0.5
              , _elapsedSinceLastStep = 0
              , _gen = g
              }
