module View exposing (view)

import Model exposing (..)

import Set exposing (Set)
import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

view : Game -> Html Msg
view model =
  div [] [ 
    div [ class "container" ] [ viewBoard model.select model.board ],
    div [ class "container" ]
      [ button [ class (if model.select then "select selected" else "select")
               , onClick SelectToggle ] [ text "Select" ]
      , button [ class "solve", onClick Solve ] [ text "Solve" ]
      , button [ class "clear", onClick Clear ] [ text "Clear" ] ] ]

viewBoard : Bool -> Board -> Html Msg
viewBoard selectMode model =
  div [ class "board" ]
    <| Array.toList 
    <| Array.indexedMap (viewCell selectMode) model

viewCell : Bool -> CellIndex -> Cell -> Html Msg
viewCell selectMode index { selected, hints, valid } = 
  case selected of
    Just val -> 
      div (List.concat
        [ [ class <| if valid then "cell filled-cell" else "cell filled-cell invalid" ]
        , if selectMode then [onClick (Deselect index)] else [] ])
        [ text (toString val) ]
    Nothing -> 
      div [ class "cell empty-cell" ] 
        <| List.map (viewHint selectMode index hints) 
        <| List.range 1 9

viewHint : Bool -> CellIndex -> Set Hint -> Hint -> Html Msg
viewHint selectMode cellIndex hints hint = 
  let 
    selected = Set.member hint hints
    action = if selectMode 
      then (Select hint cellIndex) 
      else if selected 
        then RemoveHint hint cellIndex 
        else InsertHint hint cellIndex
  in
    div [ class (if selected then "hint selected" else "hint")
        , onClick action ] 
      [ text (toString hint) ]
