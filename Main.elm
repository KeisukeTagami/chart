
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg
import Utils

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

type alias Model =
    { entries : List String
    , field : String
    , uid : Int
    , visibility : String
    }

init : ( Model, Cmd Msg )
init = emptyModel ! []

emptyModel : Model
emptyModel =
  { entries = []
  , visibility = "All"
  , field = ""
  , uid = 0
  }

view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    , Svg.svg [] []
    ]


type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment -> { model | uid = model.uid + 1
                 , field = Utils.log <| model.field ++ "a" } ! []
    Decrement -> { model | uid = model.uid - 1
                 , field = Utils.log <| model.field } ! []
