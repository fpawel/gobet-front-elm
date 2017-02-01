module Routing exposing (..)

import UrlParser exposing (Parser, map, (<?>), (</>), s, int, string, parseHash, oneOf)


-- import Navigation exposing (Location)


type Route
    = Sport Int


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Sport (s "sport" </> int)
        ]
