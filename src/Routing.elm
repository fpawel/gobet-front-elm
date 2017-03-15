module Routing exposing (..)

import UrlParser exposing (Parser, map, (<?>), (</>), s, int, string, parseHash, oneOf, custom)


--import Regex exposing (HowMany(..), find, regex)
-- import String exposing (toInt)
-- import Navigation exposing (Location)


type Route
    = Sport Int
    | Event Int
    | Football


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Sport (s "sport" </> int)
        , map Football (s "football")
        , map Event (s "event" </> int)
        ]
