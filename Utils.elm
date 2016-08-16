
module Utils exposing (..)

import Native.Utils

import Svg exposing(..)

log : String -> String
log = Native.Utils.log

width : Svg a -> Int
width = Native.Utils.width

height : Svg a -> Int
height = Native.Utils.height

size : Svg a -> (Int, Int)
size = Native.Utils.size
