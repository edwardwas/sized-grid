{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           SizedGrid.Coord
import           SizedGrid.Coord.Class
import           SizedGrid.Coord.Periodic
import           SizedGrid.Grid.Class
import           SizedGrid.Grid.Focused
import           SizedGrid.Grid.Grid
import           SizedGrid.Ordinal

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Data.AffineSpace
import           Data.Semigroup                     (Semigroup (..))
import           Generics.SOP                       hiding (S, Z)
import           GHC.TypeLits
import           Graphics.Gloss.Interface.Pure.Game

data TileState
    = Alive
    | Dead
    deriving (Eq, Show)

flipTileState :: TileState -> TileState
flipTileState Alive = Dead
flipTileState Dead  = Alive

newtype Rule (n :: Nat) = Rule
    { runRule :: TileState -> [TileState] -> TileState
    }

gameOfLife :: Rule 2
gameOfLife = Rule $ \here neigh ->
    let aliveNeigh = length $ filter (== Alive) neigh
    in if | here == Alive && aliveNeigh `elem` [2,3] -> Alive
          | here == Dead && aliveNeigh == 3 -> Alive
          | otherwise -> Dead

applyRule ::
       ( All Monoid cs
       , All IsCoord cs
       , All Semigroup cs
       , AllDiffSame Integer cs
       , All Eq cs
       , Length cs ~ n
       , All AffineSpace cs
       , KnownNat (MaxCoordSize cs)
       )
    => Rule n
    -> FocusedGrid cs TileState
    -> FocusedGrid cs TileState
applyRule rule =
    extend $ \fg ->
        runRule rule (extract fg) $
        map (\p -> peek p fg) $ filter (/= pos fg) $ moorePoints (1 :: Integer) $ pos fg

data DisplayInfo = DisplayInfo {
  tileSize :: Float
  , offset :: Float
}

data WorldState cs = WorldState
    { _grid                     :: Grid cs TileState
    , _timeElapsedSinceLastTick :: Float
    , _rule                     :: Rule (Length cs)
    , _isTicking                :: Bool
    }
makeLenses ''WorldState

gridPositionFromScreenCoord ::
       ( IsCoord x
       , IsCoord y
       )
    => DisplayInfo
    -> Float
    -> Float
    -> Maybe (Coord '[ x, y])
gridPositionFromScreenCoord DisplayInfo{..} x y =
    let x' :: Integer = floor ((x + 0.5*tileSize + offset ) / tileSize)
        y' :: Int = floor ((y + 0.5 * tileSize + offset ) / tileSize)
    in (\a b ->
            Coord
                (I (view (re asOrdinal) a) :* I (view (re asOrdinal) b) :* Nil)) <$>
       numToOrdinal x' <*>
       numToOrdinal y'

drawWorld ::
       ( cs ~ '[ a, b]
       , All Monoid cs
       , All IsCoord cs
       , All Semigroup cs
       , All AffineSpace cs
       , All Integral (MapDiff cs)
       )
    => DisplayInfo
    -> WorldState cs
    -> Picture
drawWorld DisplayInfo{..} ws =
    let image Alive = color black $ rectangleSolid tileSize tileSize
        image Dead  = color black $ rectangleWire tileSize tileSize
    in ifoldMapOf
           (grid . itraversed)
           (\p a ->
                let (x, y) = p .-. mempty
                in translate (tileSize * fromIntegral x) (tileSize * fromIntegral y) $
                   image a)
           ws

updateWorld :: forall x y .
       ( IsCoord x
       , IsCoord y
       , Semigroup x
       , Semigroup y
       , AffineSpace x
       , AffineSpace y
       , Monoid x
       , Monoid y
       , Show x
       , Show y
       , KnownNat ((*) (CoordSized x) (CoordSized y))
       )
    => DisplayInfo
    -> Event
    -> WorldState '[ x, y]
    -> WorldState '[ x, y]
updateWorld di (EventKey (MouseButton LeftButton) Up _ (x, y)) world =
    case gridPositionFromScreenCoord di x y of
        Just p  -> world & grid . gridIndex p %~ flipTileState
        Nothing -> world
updateWorld _ (EventKey (Char 't') Up _ _) world = world & isTicking %~ not
updateWorld _ _ world = world

tickWorld ::
       ( All Monoid cs
       , All Semigroup cs
       , All IsCoord cs
       , All Eq cs
       , AllDiffSame Integer cs
       , All AffineSpace cs
       , KnownNat (MaxCoordSize cs)
       )
    => Float
    -> WorldState cs
    -> WorldState cs
tickWorld dt world
    | world ^. timeElapsedSinceLastTick + dt >= 0.25 && world ^. isTicking  =
        world & grid . asFocusedGrid %~ applyRule (world ^. rule) &
        timeElapsedSinceLastTick +~
        (dt - 0.25)
    | world ^. isTicking = world & timeElapsedSinceLastTick +~ dt
    | otherwise = world

main :: IO ()
main =
    let startGame :: WorldState '[ Periodic 60, Periodic 60] =
            WorldState
            { _grid = pure Dead
            , _timeElapsedSinceLastTick = 0
            , _rule = gameOfLife
            , _isTicking = False
            }
        di = DisplayInfo {tileSize = 16, offset = 500}
    in play
           (InWindow "floatMe" (1100, 1100) (1, 1))
           white
           60
           startGame
           (\w -> translate (negate $ offset di) (negate $ offset di) $ drawWorld di w)
           (updateWorld di)
           tickWorld
