{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import           SizedGrid                    hiding (All, Compose)

import           Control.Applicative
import           Control.Comonad.Trans.Cofree
import           Control.Lens                 (ifoldMap, (&), (.~))
import           Control.Monad
import           Data.Foldable                (toList)
import           Data.Functor.Compose
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.Maybe
import           Data.Monoid                  (All (..), Any (..))

newtype Symbol = Symbol (Ordinal 9)
  deriving (Eq,Show,Ord,Enum,Bounded)

displaySymbol :: Maybe Symbol -> String
displaySymbol (Just (Symbol n)) = show $ 1 + ordinalToNum n
displaySymbol _                 = "_"

type Board = Grid '[Ordinal 9, Ordinal 9] (Maybe Symbol)

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

rows :: Board -> [Grid '[ Ordinal 1, Ordinal 9] (Maybe Symbol)]
rows = gridWindows

columns :: Board -> [Grid '[ Ordinal 9, Ordinal 1] (Maybe Symbol)]
columns = mapLowerDim gridWindows

squares :: Board -> [Grid '[ Ordinal 3, Ordinal 3] (Maybe Symbol)]
squares b = do
    a :: Grid '[ Ordinal 3, Ordinal 9] (Maybe Symbol) <- gridWindows b
    mapLowerDim gridWindows a

rowAtPoint ::
       Coord '[ Ordinal 9, Ordinal 9]
    -> Board
    -> Grid '[ Ordinal 1, Ordinal 9] (Maybe Symbol)
rowAtPoint (x :| y) b = rows b !! (ordinalToNum x)

columAtPoint (x :| y :| _) b = columns b !! (ordinalToNum y)

squareAtPoint (x :| y :| _) b =
    squares b !! (3 * ((ordinalToNum x) `mod` 3) + (ordinalToNum y) `mod` 3)

withAllSlices ::
       Monoid x
    => (forall f. Foldable f =>
                      f (Maybe Symbol) -> x)
    -> Board
    -> x
withAllSlices func b =
    mconcat (map func (rows b) ++ map func (columns b) ++ map func (squares b))

allUnique :: Eq a => [a] -> Bool
allUnique []     = True
allUnique (a:as) = all (/= a) as && allUnique as

sliceSolved :: Eq a => [Maybe a] -> Bool
sliceSolved as = all isJust as && allUnique as

gameIsSolverd = getAll . withAllSlices (All . sliceSolved . toList)

gameIsInvalid = getAny . withAllSlices (Any . not . allUnique . toList)

allValues :: Board -> Grid '[ Ordinal 9, Ordinal 9] [Symbol]
allValues b =
    let helper Nothing  = [minBound .. maxBound]
        helper (Just x) = [x]
     in helper <$> b

main = undefined

