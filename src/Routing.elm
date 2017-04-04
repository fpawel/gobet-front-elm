module Routing exposing (..)

import UrlParser exposing (Parser, map, (<?>), (</>), s, int, string, parseHash, oneOf, custom)
import Navigation exposing (Location)


--import Regex exposing (HowMany(..), find, regex)
-- import String exposing (toInt)
-- import Navigation exposing (Location)


type Route
    = RouteSport Int
    | RouteEvent Int
    | RouteFootball


parseRoute : Location -> Route
parseRoute =
    parseHash
        (oneOf
            [ map RouteSport (s "sport" </> int)
            , map RouteFootball (s "football")
            , map RouteEvent (s "event" </> int)
            ]
        )
        >> Maybe.withDefault RouteFootball
