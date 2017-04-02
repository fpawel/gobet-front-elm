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
    , sports : Dict Int Data.Aping.Sport
    , events : Dict Int Data.Aping.Event
    , markets : Dict Int Data.Aping.Market
    , sportEvents : Dict Int (List Int)
    , eventMarkets : Dict Int (List Int)
    , sportTableState : Table.State
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
    | ToggleMarket String
    | WebDataError String
