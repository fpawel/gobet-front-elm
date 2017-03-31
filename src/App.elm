module App exposing (..)

import Dict exposing (Dict)
import Navigation exposing (Location)
import Data.Football
import Data.Aping
import Table
import Prices
import Routing exposing (Route)


type alias Model =
    { rootLocation : Location
    , route : Route
    , footballGames : List Data.Football.Game
    , sports : List Data.Aping.Sport
    , events : Dict Int (List Data.Aping.Event)
    }


type Msg
    = LocationChanged Location
    | FootballWebData Data.Football.WebData
    | SportsWebData (List Data.Aping.Sport)
    | EventsWebData Int (List Data.Aping.Event)
    | EventWebData Data.Aping.Event
    | MarketPricesWebData Prices.Market
    | PricesSessionIDWebData String
    | SportTableState Table.State
    | ToggleMarket String Bool
    | WebDataError String



--getSportOfEventID : Model -> Int -> Maybe Data.Aping.Sport


getSportOfEventID : Model -> Int -> Maybe Int
getSportOfEventID { events } eventID =
    events
        |> Dict.filter
            (\sportID events ->
                events
                    |> List.filter (.id >> ((==) eventID))
                    |> List.isEmpty
                    |> not
            )
        |> Dict.toList
        |> List.map Tuple.first
        |> List.head
