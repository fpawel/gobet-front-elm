module Aping exposing (Sport, Event, getSportByID)

import Date


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


getSportByID : Int -> List Sport -> Sport
getSportByID sportID sports =
    case List.filter (\{ id } -> id == sportID) sports of
        sport :: _ ->
            sport

        _ ->
            Debug.crash <| "unknown sport id " ++ toString sportID
