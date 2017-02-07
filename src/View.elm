module View exposing (view)

import Types exposing (..)

import Set exposing (Set)
import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick)

noSelect : List ( String, String )
noSelect = List.map (\prop -> (prop, "none")) 
  [ "-webkit-touch-callout", "-webkit-user-select"
  , "-khtml-user-select", "-moz-user-select"
  , "-ms-user-select", "user-select" ]

containerStyle : List ( String, String )
containerStyle = ("font-family", "Helvetica")::noSelect

view : Board -> Html Msg
view model =
  div [ style containerStyle ] 
      [ viewBoard model
      , button [ onClick Solve ] [ text "Solve" ]
      , button [ onClick Clear ] [ text "Clear" ]]

boardStyle : List ( String, String )
boardStyle = [ ("width", "558px"), ("height", "558px"), ("cursor", "pointer")
             , ("text-align", "center"), ("outline-style", "none") ]

viewBoard : Board -> Html Msg
viewBoard model =
  div [ style boardStyle ]
    (Array.toList (Array.indexedMap viewCell model))

filledCellStyle : List ( String, String )
filledCellStyle = [ ("width", "60px"), ("height", "60px"), ("float", "left")
                  , ("font-size", "60px"), ("line-height", "60px")
                  , ("border", "1px solid black") ]

emptyCellStyle : List ( String, String )
emptyCellStyle = [ ("width", "60px"), ("height", "60px"), ("float", "left")
                 , ("font-size", "60px"), ("line-height", "60px")
                 , ("border", "1px solid black") ]

viewCell : CellIndex -> Cell -> Html Msg
viewCell index { selected, hints } = 
  case selected of
    Just val -> 
      div [ style filledCellStyle
          , onClick (Deselect index) ] 
        [ text (toString val) ]
    Nothing -> 
      div [ style emptyCellStyle ] 
        (List.map (viewHint index hints) (List.range 1 9))

hintStyle : Bool -> List ( String, String )
hintStyle selected = [ ("width", "20px"), ("height", "20px"), ("float", "left")
                  , ("font-size", "20px"), ("line-height", "20px")
                  , ("color", if selected then "black" else "lightgray") ]
      
viewHint : CellIndex -> Set Hint -> Hint -> Html Msg
viewHint cellIndex hints hint = 
  let 
    selected = Set.member hint hints
    action = if Set.member hint hints then RemoveHint hint cellIndex else InsertHint hint cellIndex
  in
    div [ style (hintStyle selected)
        , onClick action, onDoubleClick (Select hint cellIndex)] 
      [ text (toString hint) ]
