{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           SizedGrid.Coord
import           SizedGrid.Coord.Periodic
import           SizedGrid.Grid.Class
import           SizedGrid.Grid.Focused
import           SizedGrid.Grid.Grid
import           SizedGrid.Peano
import           SizedGrid.Type.Number

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Control.Monad.Random
import           Data.AdditiveGroup
import           Generics.SOP
import qualified GHC.TypeLits             as GHC
import           Pipes
import qualified Pipes.Prelude            as P
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

type GridType = '[Periodic (AsPeano 20), Periodic (AsPeano 20)]

gridSize = demoteSPeano (sPeano :: SPeano (MaxCoordSize GridType))

randomGrid ::
     (GHC.KnownNat (PeanoToNat (MaxCoordSize cs)), MonadRandom m)
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

main =
  let po = PhysicalOptions 10
  in P.toListM $ runSimulation po 10 >-> P.map (totalEnergy po)
