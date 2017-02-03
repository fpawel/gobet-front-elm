module Routing exposing (..)

import UrlParser exposing (Parser, map, (<?>), (</>), s, int, string, parseHash, oneOf)


-- import Navigation exposing (Location)


type Route
    = Sport Int
    | Football


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Sport (s "sport" </> int)
        , map Football (s "football")
        ]
