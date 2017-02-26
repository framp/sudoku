port module State exposing (..)

import Set exposing (Set)
import Array exposing (Array)
import Maybe exposing (map, withDefault)
import Model exposing (..)

port solve : Array Int -> Cmd msg
port save : String -> Cmd msg
port solutions : (List (Array Int) -> msg) -> Sub msg

init : Flags -> (Game, Cmd Msg)
init flags = 
  let 
    heartBoard = (toBoard heart)
    savedBoard = Result.withDefault heartBoard (decodeBoard flags)
    board = if isEmptyBoard savedBoard then heartBoard else savedBoard
  in
    ({ board = validateBoard board, select = False }, Cmd.none)

update : Msg -> Game -> (Game, Cmd Msg)
update msg model =
  let
    insertHint number cell = { cell | hints = Set.insert number cell.hints }
    removeHint number cell = { cell | hints = Set.remove number cell.hints }
    select index number cell = { cell | selected = Just number }
    deselect cell = { cell | selected = Nothing }
    transformElement index transform array = 
        Array.get index array 
          |> Maybe.map transform 
          |> Maybe.map (\val -> Array.set index val array)
          |> Maybe.withDefault array
    saveGame game = (game, save <| encodeBoard game.board)
  in
    case msg of
      InsertHint number cellIndex -> saveGame { model | board =
        (transformElement cellIndex (insertHint number) model.board) }

      RemoveHint number cellIndex -> saveGame { model | board =
        (transformElement cellIndex (removeHint number) model.board) }

      Select number cellIndex -> saveGame { model | board =
        validateBoard (transformElement cellIndex (select cellIndex number) model.board) }
      
      Deselect cellIndex -> saveGame { model | board =
        validateBoard (transformElement cellIndex deselect model.board) }

      Clear -> saveGame { model | board = emptyBoard }

      Solve -> (model, solve (fromBoard model.board))

      SelectToggle -> ({ model | select = not model.select }, Cmd.none)

      Solutions [first] -> saveGame { model | board = toBoard first }

      Solutions _ -> (model, Cmd.none)

subscriptions : Game -> Sub Msg
subscriptions model =
  solutions Solutions
