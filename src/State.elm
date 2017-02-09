port module State exposing (..)

import Set exposing (Set)
import Array exposing (Array)
import Maybe exposing (map, withDefault)
import Types exposing (..)

port solve : Array Int -> Cmd msg
port solutions : (List (Array Int) -> msg) -> Sub msg

init : (Board, Cmd Msg)
init = (toBoard heart, Cmd.none)

update : Msg -> Board -> (Board, Cmd Msg)
update msg model =
  case msg of
    InsertHint number cellIndex ->
      (transformElement cellIndex (insertHint number) model, Cmd.none)

    RemoveHint number cellIndex ->
      (transformElement cellIndex (removeHint number) model, Cmd.none)

    Select number cellIndex ->
      (transformElement cellIndex (select number) model, Cmd.none)
    
    Deselect cellIndex ->
      (transformElement cellIndex deselect model, Cmd.none)
    
    Clear -> (emptyBoard, Cmd.none)

    Solve -> (model, solve (fromBoard model))

    Solutions [first] -> (toBoard first, Cmd.none)

    Solutions _ -> (model, Cmd.none)
      
insertHint : Int -> Cell -> Cell
insertHint number cell = { cell | hints = Set.insert number cell.hints }

removeHint : Int -> Cell -> Cell
removeHint number cell = { cell | hints = Set.remove number cell.hints }

select : Int -> Cell -> Cell
select number cell = { cell | selected = Just number }

deselect : Cell -> Cell
deselect cell = { cell | selected = Nothing }

transformElement : Int -> (a -> a) -> Array a -> Array a
transformElement index transform array = 
    Array.get index array 
      |> Maybe.map transform 
      |> Maybe.map (\val -> Array.set index val array)
      |> Maybe.withDefault array 

subscriptions : Board -> Sub Msg
subscriptions model =
  solutions Solutions
