module Main where

import Data.List (nub)

data Game = Game Board Pieces deriving Show
type Board = (Int, Int)
type GameHistory = [Game]

data Piece = Piece PlayerColor Position deriving Show
data PlayerColor = Red | Blue  deriving (Eq, Show)
type Position = (Int, Int)
type Pieces = [Piece]

setupPieces :: Board -> [PlayerColor] -> Pieces
setupPieces (boardLenght, _) (firstColor : secondColor : _) =
  [Piece firstColor   (1,           i) | i <- [1..boardLenght]] ++
  [Piece secondColor  (boardLenght, i) | i <- [1..boardLenght]]


isGameOver :: Game -> Bool
isGameOver (Game _ pieces) = length pieceColorsOnBoard == 1
  where
    getColor (Piece color _) = color
    pieceColorsOnBoard = nub $ map getColor pieces

-- TODO: implement
-- move :: ЧемХодим -> КудаХодим -> Game -> Maybe Game

game = Game board pieces
  where
    board = (4, 4) :: Board
    playerColors = [Red, Blue]
    pieces = setupPieces board playerColors

main :: IO ()
main = putStrLn $ show $ game

