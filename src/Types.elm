module Types exposing (..)

import Array exposing (Array)
import Set exposing (Set)

type alias Board = Array Cell
type alias Game = 
  { board: Board
  , select: Bool }
type alias Cell = 
  { selected: Maybe Int
  , hints: Set Hint }
type alias CellIndex = Int
type alias Hint = Int

type Msg
    = InsertHint Int CellIndex
    | RemoveHint Int CellIndex
    | Select Int CellIndex
    | Deselect CellIndex
    | Solve
    | SelectToggle
    | Clear
    | Solutions (List (Array Int))

toCell : Int -> Cell
toCell number = 
  { hints = Set.empty
  , selected = case number of
    0 -> Nothing
    n -> Just n }

fromCell : Cell -> Int
fromCell { selected } = 
  case selected of
    Nothing -> 0
    Just n -> n

toBoard : Array Int -> Board 
toBoard = Array.map toCell

fromBoard : Board -> Array Int
fromBoard = Array.map fromCell

emptyBoard : Board
emptyBoard = Array.repeat 81 { hints = Set.empty, selected = Nothing }

heart : Array Int
heart = Array.fromList [
  0, 0, 0,  0, 0, 0,  0, 0, 0,
  0, 2, 3,  0, 0, 0,  7, 8, 0,
  1, 0, 0,  4, 0, 6,  0, 0, 9,

  9, 0, 0,  0, 5, 0,  0, 0, 4,
  2, 0, 0,  0, 0, 0,  0, 0, 8,
  0, 1, 0,  0, 0, 0,  0, 3, 0,

  0, 0, 8,  0, 0, 0,  3, 0, 0,
  0, 0, 0,  2, 0, 9,  0, 0, 0,
  0, 0, 0,  0, 1, 0,  0, 0, 0]
