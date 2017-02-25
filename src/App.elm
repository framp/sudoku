module App exposing (main)

import Html
import Model exposing (Game, Msg, Flags)
import State exposing (init, update, subscriptions)
import View exposing (view)


main : Program Flags Game Msg
main = 
  Html.programWithFlags
      { init = init
      , update = update
      , subscriptions = subscriptions
      , view = view }


