module Aping exposing (Sport, Event)

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
