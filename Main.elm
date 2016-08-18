
import Html
import Html.App as App
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Utils
import String

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
    , width : Int
    , height : Int
    , margin : (Int, Int)
    , tickSize : Float
    }

init : ( Model, Cmd Msg )
init = emptyModel ! []

emptyModel : Model
emptyModel =
  { entries = []
  , visibility = "All"
  , field = ""
  , uid = 1
  , width = 400
  , height = 400
  , margin = (80,20)
  , tickSize = 16
  }

translate (x, y) = "translate(" ++  toString x ++ "," ++ toString y ++ ")"

toPixel x = toString x ++ "px"


type Path = M (Float, Float)
          | C (Float, Float) (Float, Float) (Float, Float)
          | L (Float, Float)
          | V Float
          | H Float

toD : Model -> List Path -> Attribute msg
toD model ps =
  let f p = case p of
              M a -> "M" ++ g a
              C a b c -> "C" ++ String.join "," [g a, g b, g c]
              L a -> "L" ++ g a
              V a -> "V" ++ x a
              H a -> "H" ++ y a
      g (a,b) = x a ++ "," ++ y b
      x a = toString a
      y a = toString (a / ( toFloat model.uid))
  in d <| String.join "," <| List.map f ps

view model =
  Html.div []
    [ Html.button [ onClick Decrement ] [ Html.text "-" ]
    , Html.button [ onClick Increment ] [ Html.text "+" ]
    , Html.div [] [ Html.text (toString model) ]
    , svg [ width <| toPixel model.width
          , height <| toPixel model.height
          ] [
           g [ transform <| translate model.margin
             ]
               ( List.concat [
                      [ Svg.path [ toD model <| [ M (0,100)
                                                , C (25,50)  (50,75)  (100,100)
                                                , C (125,50) (175,50) (200,0)
                                                ] ++
                                     List.map (\ i -> L (i, 100 * (sin (i*100 / pi)) + 100 ) ) [0..400]
                                , Attr.style "stroke: rgb(31, 119, 180); fill: none;"
                                ] []
                      , Svg.path [ toD model [ M (-model.tickSize,300.5)
                                             , H 0.5
                                             , V 0.5
                                             , H (-model.tickSize)
                                             ]
                                 , Attr.style "stroke: rgb(31, 119, 180); fill: none;"
                                 ] []
                      ]
                     , List.map (\ i ->
                                     g [ transform <| translate (0, 300 / (toFloat model.uid) * (toFloat i ) )
                                       ] [
                                      line [ stroke "#000"
                                           , x2 <| toString <| -1 * model.tickSize
                                           , y1 "0"
                                           , y2 "0"
                                           ]
                                          []
                                     , let size = Utils.size <| text' [] [t]
                                           t = text <| model.field ++ toString i
                                       in text' [ x <| toString <| -2 - model.tickSize
                                                , y "0"
                                                , dx ("-" ++ ( toString <| fst size ) )
                                                , dy ( toString <| ((toFloat <| snd size) / 4) )
                                                ] [ t ]
                                     ]
                                ) [0..model.uid]
                     ]
               )
          ]
    ]

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment -> { model | uid = model.uid + 1
                 , field = Utils.log <| model.field ++ "a" } ! []
    Decrement -> { model | uid = if model.uid == 1
                                 then model.uid
                                 else model.uid  - 1
                 , field = Utils.log <| model.field } ! []
