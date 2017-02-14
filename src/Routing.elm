module Routing exposing (..)

import UrlParser exposing (Parser, map, (<?>), (</>), s, int, string, parseHash, oneOf, custom)


--import Regex exposing (HowMany(..), find, regex)
-- import String exposing (toInt)
-- import Navigation exposing (Location)


type Route
    = Sport Int
    | Event Int
    | Football



{--
event : Parser (Route -> b) b
event =
    custom "APING_SPORTID_EVENTID" <|
        \tipe ->
            case find (AtMost 1) (regex "(\\d+)-(\\d+)") tipe of
                [ { submatches } ] ->
                    case submatches of
                        [ Just strSportIDPart, Just strEventIDPart ] ->
                            case ( toInt strSportIDPart, toInt strEventIDPart ) of
                                ( Ok sportID, Ok eventID ) ->
                                    Ok <| Event sportID eventID

                                x ->
                                    Err <| "not int's: " ++ toString x

                        x ->
                            Err <| "wrong submatches: " ++ toString x

                _ ->
                    Err "not matches with `event` pattern `(\\d+)-(\\d+)`"

-}


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Sport (s "sport" </> int)
        , map Football (s "football")
        , map Event (s "event" </> int)
        ]
