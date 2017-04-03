module App exposing (..)

import Dict exposing (Dict)
import Navigation exposing (Location)
import Data.Football
import Data.Aping
import Data.Prices
import Table


type alias Model =
    { location : Location
    , page : Page
    , footballGames : List Data.Football.Game
    , sports : Dict Int Data.Aping.Sport
    , sportEvents : Dict Int (List Int)
    , events : Dict Int Data.Aping.Event
    }


type Page
    = PageSport Table.State
    | PageEvent MarketsPrices
    | PageFootball


type alias MarketsPrices =
    { session : String
    , marketsPrices : Dict String Data.Prices.Market
    }


type Msg
    = LocationChanged Location
    | FootballWebData Data.Football.WebData
    | SportsWebData (List Data.Aping.Sport)
    | EventsWebData Int (List Data.Aping.Event)
    | PricesWebData Data.Prices.WebData
    | SportTableState Table.State
    | ToggleMarket String
    | ToggleMarketPosted String
    | WebDataError String
