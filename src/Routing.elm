module Routing exposing (..)

import UrlParser exposing (Parser, map, (<?>), (</>), s, int, string, parseHash, oneOf, custom)
import Navigation exposing (Location)


--import Regex exposing (HowMany(..), find, regex)
-- import String exposing (toInt)
-- import Navigation exposing (Location)


type Route
    = Sport Int
    | Event Int
    | Football


parse : Location -> Route
parse =
    parseHash
        (oneOf
            [ map Sport (s "sport" </> int)
            , map Football (s "football")
            , map Event (s "event" </> int)
            ]
        )
        >> Maybe.withDefault Football
