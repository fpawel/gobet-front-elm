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
    , marketsPrices : Markets
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


type alias Markets =
    Dict String Market


type alias Market =
    { id : String
    , totalMatched : Maybe Float
    , totalAvailable : Maybe Float
    , prices : Data.Prices.MarketPrices
    }


toggleMarket : String -> Dict String Market -> Dict String Market
toggleMarket marketID marketsPrices =
    marketsPrices
        |> Dict.get marketID
        |> Maybe.map (\_ -> Dict.remove marketID marketsPrices)
        |> Maybe.withDefault
            (Dict.insert
                marketID
                { id = marketID
                , totalMatched = Nothing
                , totalAvailable = Nothing
                , prices = Dict.empty
                }
                marketsPrices
            )


tryGetSportByEventID : Model -> Int -> Maybe Data.Aping.Sport
tryGetSportByEventID m eventID =
    m.sportEvents
        |> Dict.toList
        |> List.concatMap
            (\( sportID, eventIDs ) ->
                eventIDs
                    |> List.map (\eventID -> ( eventID, sportID ))
            )
        |> Dict.fromList
        |> Dict.get eventID
        |> Maybe.andThen
            (\sportID ->
                Dict.get sportID m.sports
            )
