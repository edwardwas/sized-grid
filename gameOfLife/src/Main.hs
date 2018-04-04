{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
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
import           SizedGrid.Peano
import           SizedGrid.Type.Number

import           Control.Comonad
import           Control.Comonad.Store
import           Control.Lens
import           Control.Monad.Random
import           Data.AffineSpace
import           Data.Functor.Rep
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
        map (\p -> peek p fg) $ filter (/= pos fg) $ moorePoints 1 $ pos fg

data WorldState cs = WorldState
    { _grid                     :: Grid cs TileState
    , _timeElapsedSinceLastTick :: Float
    , _rule                     :: Rule (Length cs)
    , _isTicking                :: Bool
    , _displayString            :: String
    }
makeLenses ''WorldState

gridPositionFromScreenCoord ::
       ( RealFrac a
       , IsCoord x
       , IsCoord y
       )
    => a
    -> a
    -> Maybe (Coord '[ x, y])
gridPositionFromScreenCoord x y =
    let x' = floor ((x + 16) / 32)
        y' = floor ((y + 16) / 32)
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
    => WorldState cs
    -> Picture
drawWorld ws =
    let image Alive = color black $ rectangleSolid 32 32
        image Dead  = color black $ rectangleWire 32 32
    in ifoldMapOf
           (grid . itraversed)
           (\p a ->
                let (x, y) = p .-. mempty
                in translate (fromIntegral $ 32 * x) (fromIntegral $ 32 * y) $
                   image a)
           ws `mappend`
       translate (-200) (-200) (scale 0.1 0.1 (text (ws ^. displayString)))

updateWorld :: forall x y n m .
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
    => Event
    -> WorldState '[ x, y]
    -> WorldState '[ x, y]
updateWorld (EventKey (MouseButton LeftButton) Up _ (x, y)) world =
    case gridPositionFromScreenCoord x y of
        Just p  -> world & grid . gridIndex p %~ flipTileState
        Nothing -> world
updateWorld (EventKey (Char 't') Up _ _) world = world & isTicking %~ not
updateWorld (EventMotion (x, y)) world =
    world & displayString .~
    show (x, y, gridPositionFromScreenCoord x y :: Maybe (Coord '[ x, y]))
updateWorld _ world = world

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
    | world ^. timeElapsedSinceLastTick + dt >= 1 && world ^. isTicking  =
        world & grid . asFocusedGrid %~ applyRule (world ^. rule) &
        timeElapsedSinceLastTick +~
        (dt - 1)
    | world ^. isTicking = world & timeElapsedSinceLastTick +~ dt
    | otherwise = world

main :: IO ()
main =
    let startGame :: WorldState '[ Periodic 30, Periodic 30] =
            WorldState
            { _grid = pure Dead
            , _timeElapsedSinceLastTick = 0
            , _rule = gameOfLife
            , _isTicking = False
            , _displayString = ""
            }
    in play
           (InWindow "floatMe" (960, 960) (1, 1))
           white
           60
           startGame
           (\w -> drawWorld w)
           updateWorld
           tickWorld
