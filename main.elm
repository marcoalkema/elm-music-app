import StartApp.Simple exposing (start)
import Html exposing (..)
import Signal exposing (Address)
import Piano exposing (..)

main : Signal Html
main =
  start
    { model = (0, 1, [12])
    , update = update
    , view = view
    }
