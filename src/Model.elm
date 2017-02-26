module Model exposing (..)

import Array exposing (Array)
import Set exposing (Set)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)

type alias Board = Array Cell
type alias Game = 
  { board: Board
  , select: Bool }
type alias Cell = 
  { selected: Maybe Int
  , hints: Set Hint
  , valid: Bool }
type alias CellIndex = Int
type alias Hint = Int
type alias Flags = String

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
    n -> Just n 
  , valid = False }

fromCell : Cell -> Int
fromCell { selected } = 
  case selected of
    Nothing -> 0
    Just n -> n

toBoard : Array Int -> Board 
toBoard = Array.map toCell >> validateBoard

fromBoard : Board -> Array Int
fromBoard = Array.map fromCell

set : Decoder comparable -> Decoder (Set comparable)
set decoder = (Json.Decode.list decoder) 
  |> andThen (Set.fromList >> succeed)

cellDecoder : Decoder Cell
cellDecoder = decode Cell
    |> required "selected" (nullable Json.Decode.int)
    |> required "hints" (set Json.Decode.int)
    |> hardcoded True

boardDecoder : Decoder Board
boardDecoder = Json.Decode.array cellDecoder

decodeBoard : String -> Result String Board
decodeBoard = decodeString boardDecoder

cellEncoder : Cell -> Json.Encode.Value
cellEncoder { selected, hints } =
  Json.Encode.object 
    [ ("selected", case selected of
      Nothing -> Json.Encode.null
      Just n -> Json.Encode.int n) 
    , ("hints", Json.Encode.list 
      <| List.map Json.Encode.int (Set.toList hints) )]

boardEncoder : Array Cell -> Json.Encode.Value
boardEncoder board = 
  Json.Encode.array 
    <| Array.map cellEncoder board

encodeBoard : Board -> String
encodeBoard board = encode 0 (boardEncoder board)

validateBoard : Board -> Board
validateBoard board = 
  let
    indexToLine index = List.map (\n -> index // 9 * 9 + n) (List.range 0 8)
    indexToColumn index = List.map (\n -> index % 9 + 9 * n) (List.range 0 8)
    indexToRegion index = List.map (\n -> (index // 9 // 3) * 3 * 9 + 
                                          ((index % 9) // 3) * 3 + 
                                          n % 3 + n // 3 * 9) (List.range 0 8)
    inSet indexTransform (index, cell) board =
      indexTransform index 
      |> List.filter ((/=) index)
      |> List.map (flip Array.get board 
               >> Maybe.map .selected
               >> Maybe.map (Maybe.withDefault 0)
               >> Maybe.withDefault 0)
      |> List.member ((Maybe.withDefault 0) cell.selected)
    valid (index, cell) board = not 
      ((inSet indexToLine (index, cell) board) ||
      (inSet indexToColumn (index, cell) board) ||
      (inSet indexToRegion (index, cell) board))

    updateValidity (index, cell) =
      Array.set index { cell | valid = valid (index, cell) board }
  in
    Array.foldl updateValidity board <| Array.indexedMap (,) board

emptyBoard : Board
emptyBoard = Array.repeat 81 { hints = Set.empty, selected = Nothing, valid = False }

isEmptyBoard : Board -> Bool
isEmptyBoard board = List.all (\{ selected } -> selected == Nothing) <| Array.toList board

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
