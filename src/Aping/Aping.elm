module Aping exposing (Sport, Event, Market, getSportByID, eventTeams)

import Date
import Regex exposing (..)


type alias Sport =
    { id : Int
    , name : String
    , market_count : Int
    }


type alias Event =
    { id : Int
    , name : String
    , country : String
    , openDate : Date.Date
    , timezone : String
    , venue : String
    }


type alias Market =
    { id : Int
    , name : String
    , totalMatched : Float
    , runners : List Runner
    , competition : String
    }


type alias Runner =
    { id : Int
    , name : String
    }


getSportByID : Int -> List Sport -> Sport
getSportByID sportID sports =
    case List.filter (\{ id } -> id == sportID) sports of
        sport :: _ ->
            sport

        _ ->
            Debug.crash <| "unknown sport id " ++ toString sportID


eventTeams : String -> Maybe ( String, String )
eventTeams s =
    case split (AtMost 1) (regex " [v@\\-] ") s of
        [ s1, s2 ] ->
            Just ( s1, s2 )

        _ ->
            Nothing
