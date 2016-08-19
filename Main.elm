
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
    , xTickCount : Int
    , yTickCount : Int
    , dragStartAt : Maybe (Int, Int)
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
  , yMax = 100
  , yMin = -10
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

toX model a = ( a - model.xMin ) / ( model.xMax - model.xMin) * ( toFloat ( model.width - ( fst model.margin ) * 2 ) )
toY model a = ( 1.0 - ( a - model.yMin ) / ( model.yMax - model.yMin) ) * ( toFloat ( model.height - ( snd model.margin ) * 2 ) )

yAxis model = ( Svg.path [ toD (toX model) (toY model) [ M (0 , model.yMax)
                                                       , V model.yMin
                                                       ]
                         , Attr.style "stroke: rgb(31, 119, 180); fill: none;"
                         ] []
              ) :: ( List.map (\ i ->
                                   let j = (model.yMax - model.yMin) * toFloat i / toFloat model.yTickCount + model.yMin
                                   in g [ transform <| translate (0, toY model j)
                                        ]
                                   [ line [ stroke "#000"
                                          , x1 <| toString <| toX model model.xMax
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
xAxis model = ( Svg.path [ toD (toX model) (toY model) [ M (model.xMin, 0)
                                                       , H model.xMax
                                                       ]
                         , Attr.style "stroke: rgb(31, 119, 180); fill: none;"
                         ] []
              ) :: ( List.map (\ i ->
                                   let j = (model.xMax - model.xMin) * toFloat i / toFloat model.xTickCount + model.xMin
                                   in g [ transform <| translate (toX model j, toY model model.yMin)
                                        ]
                                   [ line [ stroke "#000"
                                          , x1 "0"
                                          , x2 "0"
                                          , y1 <| toString <| -1 * ( toY model ( model.yMin + model.yMin) )
                                          , y2 <| toString <| model.tickSize
                                          ]
                                       []
                                   , let size = Utils.size <| text' [] [t]
                                         t = text <| Formatting.print (Formatting.roundTo 2) j
                                     in text' [ x "0"
                                              , y "0"
                                              , dx ("-" ++ ( toString <| (toFloat ( fst size) / 2) ) )
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
                      ( "clientX" := Decode.int)
                      ( "clientY" := Decode.int)
                )
          , onWithOptions
                "mouseup"
                { stopPropagation = True, preventDefault = True }
                ( Decode.object2 MouseUp
                      ( "clientX" := Decode.int)
                      ( "clientY" := Decode.int)
                )
          , onWithOptions
                "mousemove"
                { stopPropagation = True, preventDefault = True }
                ( Decode.object2 MouseMove
                      ( "clientX" := Decode.int)
                      ( "clientY" := Decode.int)
                )
          ] [
           g [ transform <| translate model.margin
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

type Msg = Increment | Decrement | Wheel Float | MouseDown Int Int | MouseUp Int Int | MouseMove Int Int

update msg model =
  case msg of
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
    MouseDown x y -> { model | field = toString x ++ " down " ++ toString y
                     , dragStartAt = Just (x, y)
                     } ! []
    MouseUp x y -> { model | field = toString x ++ " up " ++ toString y
                   , dragStartAt = Nothing } ! []
    -- MouseMove x y -> case model.dragStartAt of
    --                      Just _ -> { model | field = toString x ++ " move " ++ toString y} ! []
    --                      Nothing -> model ! []
    MouseMove x y -> { model | field = toString x ++ " move " ++ toString y} ! []
