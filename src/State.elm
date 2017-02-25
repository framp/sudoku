port module State exposing (..)

import Set exposing (Set)
import Array exposing (Array)
import Maybe exposing (map, withDefault)
import Model exposing (..)

port solve : Array Int -> Cmd msg
port solutions : (List (Array Int) -> msg) -> Sub msg

init : (Game, Cmd Msg)
init = ({ board = toBoard heart, select = False }, Cmd.none)

update : Msg -> Game -> (Game, Cmd Msg)
update msg model =
  case msg of
    InsertHint number cellIndex -> ({ model | board =
      (transformElement cellIndex (insertHint number) model.board) }, Cmd.none)

    RemoveHint number cellIndex -> ({ model | board =
      (transformElement cellIndex (removeHint number) model.board) }, Cmd.none)

    Select number cellIndex -> ({ model | board =
      validateBoard (transformElement cellIndex (select cellIndex number) model.board) }, Cmd.none)
    
    Deselect cellIndex -> ({ model | board =
      validateBoard (transformElement cellIndex deselect model.board) }, Cmd.none)
    
    Clear -> ({ model | board = emptyBoard }, Cmd.none)

    Solve -> (model, solve (fromBoard model.board))

    SelectToggle -> ({ model | select = not model.select }, Cmd.none)

    Solutions [first] -> ({ model | board = toBoard first }, Cmd.none)

    Solutions _ -> (model, Cmd.none)

      
insertHint : Int -> Cell -> Cell
insertHint number cell = { cell | hints = Set.insert number cell.hints }

removeHint : Int -> Cell -> Cell
removeHint number cell = { cell | hints = Set.remove number cell.hints }

select : CellIndex -> Int -> Cell -> Cell
select index number cell = { cell | selected = Just number }

deselect : Cell -> Cell
deselect cell = { cell | selected = Nothing }

transformElement : Int -> (a -> a) -> Array a -> Array a
transformElement index transform array = 
    Array.get index array 
      |> Maybe.map transform 
      |> Maybe.map (\val -> Array.set index val array)
      |> Maybe.withDefault array 

subscriptions : Game -> Sub Msg
subscriptions model =
  solutions Solutions
