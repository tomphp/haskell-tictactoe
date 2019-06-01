module TicTacToe.Coordinate
  ( Coordinate
  , allColumns
  , allCoordinates
  , allDiagonals
  , allRows
  , column
  , fromIndex
  , new
  , row
  , toTuple
  ) where

import Control.Monad.Except (MonadError, throwError)

data Coordinate = Coordinate Int Int deriving (Eq, Show)

rowCount :: Int
rowCount = 3

columnCount :: Int
columnCount = 3

new :: MonadError () m => Int -> Int -> m Coordinate
new r c 
  | r < 1           = throwError ()
  | c < 1           = throwError ()
  | r > rowCount    = throwError ()
  | c > columnCount = throwError ()
  | otherwise       = return $ Coordinate r c

fromIndex :: MonadError () m => Int -> m Coordinate
fromIndex i = new r c
  where r = succ (i' `div` columnCount)
        c = succ (i' `mod` columnCount)
        i' = pred i

toTuple :: Coordinate -> (Int, Int)
toTuple (Coordinate r c) = (r, c)

row :: Coordinate -> Int
row (Coordinate r _) = r

column :: Coordinate -> Int
column (Coordinate _ c) = c

allCoordinates :: [Coordinate]
allCoordinates = [Coordinate r c | r <- [1..rowCount], c <- [1..columnCount]]

allRows :: [[Coordinate]]
allRows = [[Coordinate r c | c <- [1..columnCount] ] | r <- [1..rowCount]]
 
allColumns :: [[Coordinate]]
allColumns = [[Coordinate r c | r <- [1..rowCount] ] | c <- [1..columnCount]]
 
allDiagonals :: [[Coordinate]]
allDiagonals =
    [ uncurry Coordinate <$> [(1, 1), (2, 2), (3, 3)] -- too specific
    , uncurry Coordinate <$> [(1, 3), (2, 2), (3, 1)] -- too specific
    ]

