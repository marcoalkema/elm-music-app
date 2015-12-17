import StartApp.Simple exposing (start)
import Html exposing (..)
import Piano exposing (..)

main : Signal Html
main =
  start
    { model = init
    , update = update
    , view = view
    }

