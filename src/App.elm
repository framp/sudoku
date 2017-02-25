module App exposing (main)

import Html
import Model exposing (Game, Msg)
import State exposing (init, update, subscriptions)
import View exposing (view)


main : Program Never Game Msg
main = 
  Html.program
      { init = init
      , update = update
      , subscriptions = subscriptions
      , view = view }


