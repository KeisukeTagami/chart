
import Html
import Html.App as App
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as Attr exposing (..)
import Utils
import String
import Formatting
import Json.Decode as Decode exposing ((:=))

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
    , base : (Float, Float)
    , xTickCount : Int
    , yTickCount : Int
    , dragStartAt : Maybe ( (Int, Int), (Float, Float) )
    }

init : ( Model, Cmd Msg )
init = emptyModel ! []

emptyModel : Model
emptyModel =
  { field = ""
  , width = 800
  , height = 400
  , margin = (80,30)
  , tickSize = 6
  , xMax = 600
  , xMin = -100
  , yMax = 600
  , yMin = -100
  , base = (0, 0)
  , xTickCount = 5
  , yTickCount = 5
  , dragStartAt = Nothing
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

toX model a =
    let xMin = model.xMin + fst model.base
        xMax = model.xMax + fst model.base
        yMin = model.yMin + snd model.base
        yMax = model.yMax + snd model.base
    in ( a - xMin ) / ( xMax - xMin) * ( chartWidth model )
toY model a =
    let xMin = model.xMin + fst model.base
        xMax = model.xMax + fst model.base
        yMin = model.yMin + snd model.base
        yMax = model.yMax + snd model.base
    in ( 1.0 - ( a - yMin ) / ( yMax - yMin) ) * ( chartHeight model )

fromX model a =
    let xMin = model.xMin + fst model.base
        xMax = model.xMax + fst model.base
        yMin = model.yMin + snd model.base
        yMax = model.yMax + snd model.base
    in a * ( xMax - xMin) / ( chartWidth model ) + xMin
fromY model a =
    let b = 1.0 - (a / chartHeight model )
        y = b * ( yMax - yMin ) + yMin
        xMin = model.xMin + fst model.base
        xMax = model.xMax + fst model.base
        yMin = model.yMin + snd model.base
        yMax = model.yMax + snd model.base
    in y

chartWidth model = toFloat ( model.width - ( fst model.margin ) * 2 )
chartHeight model = toFloat ( model.height - ( snd model.margin ) * 2 )

yAxis model = ( Svg.path [ toD (toX model) (toY model) [ M (0 , model.yMax + snd model.base)
                                                       , V (model.yMin + snd model.base)
                                                       ]
                         , Attr.style "stroke: rgb(31, 119, 180); fill: none;"
                         ] []
              ) :: ( List.map (\ i ->
                                   let j = (model.yMax - model.yMin) * toFloat i / toFloat model.yTickCount + model.yMin + snd model.base
                                   in g [ transform <| translate (0, toY model j)
                                        ]
                                   [ line [ stroke "#000"
                                          , x1 <| toString <| toX model ( model.xMax + fst model.base )
                                          , x2 <| toString <| -1 * model.tickSize
                                          , y1 "0"
                                          , y2 "0"
                                          ]
                                       []
                                   , let size = Utils.size <| text' [] [t]
                                         t = text <| Formatting.print (Formatting.roundTo 2) j
                                     in text' [ x <| toString <| -2 - model.tickSize
                                              , y "0"
                                              , dx ("-" ++ ( toString <| fst size ) )
                                              , dy ( toString <| ((toFloat <| snd size) / 4) )
                                              ] [ t ]
                                   ]
                                ) [0..model.yTickCount]
                   )
xAxis model = ( Svg.path [ toD (toX model) (toY model) [ M (model.xMin + fst model.base, 0)
                                                       , H (model.xMax + fst model.base)
                                                       ]
                         , Attr.style "stroke: rgb(31, 119, 180); fill: none;"
                         ] []
              ) :: ( List.map (\ i ->
                                   let j = (model.xMax - model.xMin) * toFloat i / toFloat model.xTickCount + model.xMin + fst model.base
                                   in g [ transform <| translate (toX model j, toY model (model.yMin + snd model.base) )
                                        ]
                                   [ line [ stroke "#000"
                                          , x1 "0"
                                          , x2 "0"
                                          , y1 <| toString <| -1 * ( toY model ( model.yMin + snd model.base) )
                                          , y2 <| toString <| model.tickSize
                                          ]
                                       []
                                   , let size = Utils.size <| text' [] [t]
                                         t = text <| Formatting.print (Formatting.roundTo 2) j
                                     in text' [ x "0"
                                              , y "0"
                                              , dx ("-" ++ ( toString <| (toFloat (fst size) / 2) ) )
                                              , dy ( toString <| (toFloat <| snd size) + model.tickSize )
                                              ] [ t ]
                                   ]
                                ) [0..model.xTickCount]
                   )


view model =
  Html.div []
    [ Html.button [ onClick Decrement ] [ Html.text "-" ]
    , Html.button [ onClick Increment ] [ Html.text "+" ]
    , Html.div [] [ Html.text (toString model) ]
    , svg [ width <| toPixel model.width
          , height <| toPixel model.height
          , onWithOptions
                "wheel"
                { stopPropagation = True, preventDefault = True }
                ( Decode.object1 Wheel ( "deltaY" := Decode.float) )
          , onWithOptions
                "mousedown"
                { stopPropagation = True, preventDefault = True }
                ( Decode.object2 MouseDown
                      ( "offsetX" := Decode.int)
                      ( "offsetY" := Decode.int)
                )
          , onWithOptions
                "mouseup"
                { stopPropagation = True, preventDefault = True }
                ( Decode.object2 MouseUp
                      ( "offsetX" := Decode.int)
                      ( "offsetY" := Decode.int)
                )
          , onWithOptions
                "mousemove"
                { stopPropagation = True, preventDefault = True }
                ( Decode.object2 MouseMove
                      ( "offsetX" := Decode.int)
                      ( "offsetY" := Decode.int)
                )
          ] [ g [ transform <| translate model.margin
                , onWithOptions "mousedown" { stopPropagation = True, preventDefault = True } ( Decode.succeed NoOp)
                , onWithOptions "mouseup" { stopPropagation = True, preventDefault = True } ( Decode.succeed NoOp)
                , onWithOptions "mousemove" { stopPropagation = True, preventDefault = True } ( Decode.succeed NoOp)
                  ]
                    ( List.concat [
                           [ Svg.path [ toD (toX model) (toY model) <| [M (0,0)] ++ List.map (\ i -> L ( i, 100 * (sin (i / pi)) + 100 ) ) [0..400]
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

type Msg = NoOp | Increment | Decrement | Wheel Float | MouseDown Int Int | MouseUp Int Int | MouseMove Int Int

update msg model =
  let toScreenX x = (toFloat x) * ( model.xMax - model.xMin) / chartWidth model
      toScreenY y = (toFloat y) * ( model.yMax - model.yMin) / chartHeight model
  in case msg of
         NoOp -> model ! []
         Increment -> { model | yTickCount = model.yTickCount + 1
                      , field = Utils.log <| model.field ++ "a" } ! []
         Decrement -> { model | yTickCount = if model.yTickCount == 1
                                             then 1
                                             else model.yTickCount - 1
                      , field = Utils.log <| model.field } ! []
         Wheel dy -> { model | yMax = model.yMax * ( 1000 + dy ) / 1000
                     , yMin = model.yMin * ( 1000 + dy ) / 1000
                     , xMax = model.xMax * ( 1000 + dy ) / 1000
                     , xMin = model.xMin * ( 1000 + dy ) / 1000
                     } ! []
         MouseDown x y -> { model | dragStartAt = Just ( (x, y), model.base)
                          } ! []
         MouseUp x y -> { model | dragStartAt = Nothing
                        } ! []
         MouseMove x y -> case model.dragStartAt of
                              Just ((sx,sy), (i,j)) -> { model | field =
                                                             " move " ++
                                                             toString ( x - sx  ) ++
                                                             " " ++
                                                             toString ( y - sy )
                                                       , base = ( i - toScreenX (x - sx), j + toScreenX (y - sy) )
                                                       } ! []
                              Nothing -> model ! []
