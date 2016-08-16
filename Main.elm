
import Html
import Html.App as App
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
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

translate (x, y) = "translate(" ++ (toString x) ++ "," ++ (toString y) ++ ")"

view model =
  Html.div []
    [ Html.button [ onClick Decrement ] [ Html.text "-" ]
    , Html.div [] [ Html.text (toString model) ]
    , Html.button [ onClick Increment ] [ Html.text "+" ]
    , svg [ width "300px"
          , height "300px"
          ] [
           g [ transform <| translate (80,20)
             ] [
            Svg.path [ d "M0,0C25,50,50,75,100,100C125,50,175,50,200,0"
                     , Attr.style "stroke: rgb(31, 119, 180); fill: none;"
                     ] []
           , Svg.path [ d "M -6,250.5 H0.5 V0.5 H-6"
                      , Attr.style "stroke: rgb(31, 119, 180); fill: none;"
                      ] []
           , g [ transform <| translate (0,144)
               ] [
                 line [ stroke "#000"
                      , x2 "-6"
                      , y1 "0"
                      , y2 "0"
                      ]
                     []
                , let size = Utils.size <| text' [] [t]
                      t = text model.field
                  in text' [ x "-8"
                           , y "0"
                           , dx ("-" ++ ( toString <| fst size ) )
                           , dy ( toString <| ((toFloat <| snd size) / 4) )
                           ] [ t ]
                ]
           , text' [] [ text <| toString <| Utils.size <| text' [] [text "10000"] ]
           ]
          ]
    ]


type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment -> { model | uid = model.uid + 1
                 , field = Utils.log <| model.field ++ "a" } ! []
    Decrement -> { model | uid = model.uid - 1
                 , field = Utils.log <| model.field } ! []
