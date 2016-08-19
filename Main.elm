
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
    { field : String
    , width : Int
    , height : Int
    , margin : (Int, Int)
    , tickSize : Float
    , xMax : Float
    , xMin : Float
    , yMax : Float
    , yMin : Float
    , yTickCount : Int
    }

init : ( Model, Cmd Msg )
init = emptyModel ! []

emptyModel : Model
emptyModel =
  { field = ""
  , width = 800
  , height = 400
  , margin = (80,20)
  , tickSize = 16
  , xMax = 100
  , xMin = 0
  , yMax = 100
  , yMin = 0
  , yTickCount = 5
  }

translate (x, y) = "translate(" ++  toString x ++ "," ++ toString y ++ ")"

toPixel x = toString x ++ "px"

type Path = M (Float, Float)
          | C (Float, Float) (Float, Float) (Float, Float)
          | L (Float, Float)
          | V Float
          | H Float

toD : (Float -> Float) -> (Float -> Float ) -> List Path -> Attribute msg
toD x y ps =
  let f p = case p of
              M a -> "M" ++ g a
              C a b c -> "C" ++ String.join "," [g a, g b, g c]
              L a -> "L" ++ g a
              V a -> "V" ++ toString (y a)
              H a -> "H" ++ toString (x a)
      g (a,b) = toString (x a) ++ "," ++ toString (y b)
  in d <| String.join "," <| List.map f ps

toX model a = ( a - model.xMin ) / ( model.xMax - model.xMin) * ( toFloat ( model.width - fst model.margin) )
toY model a = ( 1.0 - ( a - model.yMin ) / ( model.yMax - model.yMin) ) * ( toFloat ( model.height - snd model.margin) )

yAxis model = ( Svg.path [ toD (\x->x) (toY model) [ M (-model.tickSize, model.yMax)
                                              , H 0
                                              , V 0
                                              , H (-model.tickSize)
                                              ]
                         , Attr.style "stroke: rgb(31, 119, 180); fill: none;"
                         ] []
              ) :: ( List.map (\ i ->
                                   g [ transform <| translate (0, toY model (model.yMax * toFloat i / toFloat model.yTickCount) )
                                     ]
                                   [ line [ stroke "#000"
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
                                ) [0..model.yTickCount]
                   )
xAxis model = [ Svg.path [ toD (toX model) (\y->y) [ M (0, model.tickSize)
                                                   , V 0
                                                   , H model.xMax
                                                   , V <| 0 + model.tickSize
                                                   ]
                         , Attr.style "stroke: rgb(31, 119, 180); fill: none;"
                         ] []
              ]


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
                      [ Svg.path [ toD (toX model) (toY model) <| List.map (\ i -> L ( i, 100 * (sin (i / pi)) + 100 ) ) [0..400]
                                 , Attr.style "stroke: rgb(31, 119, 180); fill: none;"
                                 ]
                          []
                      ]
                     , xAxis model
                     , yAxis model
                     ]
             )
          ]
    ]

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment -> { model | yMax = model.yMax * 2
                 , field = Utils.log <| model.field ++ "a" } ! []
    Decrement -> { model | yMax = model.yMax / 2
                 , field = Utils.log <| model.field } ! []
