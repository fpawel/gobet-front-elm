port module Main exposing (..)

import Navigation exposing (program)
import Dict exposing (Dict)
import WebSocket
import Http
import Json.Decode
import Navigation exposing (Location)
import Data.Football
import Data.Aping
import Data.Prices
import Routing exposing (Route(..), parseRoute)
import Help.Utils exposing (websocketURL, isJust, fromResult)
import App exposing (Msg(..), Page(..), Model)
import Update.ToggleMarket
import Table
import Debug exposing (crash, log)
import View exposing (view)


main : Program Never Model Msg
main =
    program LocationChanged
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        ( page, cmd ) =
            initPage location
    in
        { location = location
        , page = page
        , footballGames = []
        , sports = Dict.empty
        , sportEvents = Dict.empty
        , events = Dict.empty
        }
            ! [ cmd
              , webRequestSports location
              ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        LocationChanged location ->
            updateLocation m location

        FootballWebData webdata ->
            updateFootball webdata m

        WebDataError error ->
            logValue m "web data error" error

        SportsWebData sports ->
            { m | sports = dictID sports } ! []

        EventsWebData sportID events ->
            updateEventsWebData m sportID events

        PricesWebData webdata ->
            updateMarketsPrices m webdata

        ToggleMarket marketID ->
            Update.ToggleMarket.update m marketID

        SportTableState tableState ->
            updateSportTableState m tableState ! []

        ToggleMarketPosted marketID ->
            logValue m "ToggleMarketPosted" marketID



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
    case parseRoute m.location of
        RouteFootball ->
            WebSocket.listen
                (websocketURL m.location ++ "/football")
                (Data.Football.parseWebData
                    >> fromResult WebDataError FootballWebData
                )

        RouteEvent eventID ->
            WebSocket.listen
                (websocketURLPrices m.location eventID)
                Data.Prices.decodeWebData
                |> Sub.map
                    (fromResult WebDataError PricesWebData)

        _ ->
            Sub.none



-- HELPERS


dictID : List { a | id : comparable } -> Dict comparable { a | id : comparable }
dictID =
    List.map (\x -> ( x.id, x )) >> Dict.fromList


logValue : a -> String -> b -> ( a, Cmd msg )
logValue m text value =
    let
        _ =
            log text value
    in
        m ! []


initPage : Location -> ( Page, Cmd Msg )
initPage location =
    case parseRoute location of
        RouteFootball ->
            ( PageFootball, Cmd.none )

        RouteSport sportID ->
            ( PageSport (Table.initialSort "Дата открытия")
            , webRequestEvents sportID location
            )

        RouteEvent eventID ->
            ( PageEvent
                { session = ""
                , marketsPrices = Dict.empty
                }
            , Cmd.none
            )


updateLocation : Model -> Location -> ( Model, Cmd Msg )
updateLocation m nextLocation =
    let
        prevRoute =
            parseRoute m.location

        nextRoute =
            parseRoute nextLocation

        m_ =
            { m | location = nextLocation }
    in
        if prevRoute == nextRoute then
            m_ ! []
        else
            let
                ( page, cmd ) =
                    initPage nextLocation
            in
                { m_ | page = page } ! [ cmd ]


updatePricesSessionID : Model -> String -> String -> ( Model, Cmd Msg )
updatePricesSessionID m sessionID hashCode =
    case ( parseRoute m.location, m.page ) of
        ( RouteEvent eventID, PageEvent p ) ->
            { m | page = PageEvent { p | session = sessionID } }
                ! [ WebSocket.send
                        (websocketURLPrices m.location eventID)
                        hashCode
                  ]

        x ->
            crash ("updatePricesSessionID: " ++ toString x)


updateFootball : Data.Football.WebData -> Model -> ( Model, Cmd Msg )
updateFootball webdata m =
    let
        nextGames =
            Data.Football.updateGamesList webdata.changes m.footballGames

        answer =
            WebSocket.send
                (websocketURL m.location ++ "/football")
                webdata.hashCode
    in
        { m | footballGames = nextGames } ! [ answer ]


websocketURLPrices : Navigation.Location -> Int -> String
websocketURLPrices location eventID =
    Help.Utils.websocketURL location ++ "/wsprices/" ++ toString eventID


webRequestSports : Location -> Cmd Msg
webRequestSports location =
    let
        url =
            location.protocol ++ "//" ++ location.host ++ "/sports"

        decoder =
            Json.Decode.list Data.Aping.decoderSport
                |> Json.Decode.field "ok"
    in
        Http.get url decoder
            |> Http.send
                (Result.mapError toString
                    >> fromResult WebDataError SportsWebData
                )


webRequestEvents : Int -> Location -> Cmd Msg
webRequestEvents sportID location =
    let
        eventsURL =
            location.protocol
                ++ "//"
                ++ location.host
                ++ "/events/"
                ++ toString sportID

        decoder =
            Json.Decode.list Data.Aping.decoderEvent
                |> Json.Decode.field "ok"
    in
        Http.get eventsURL decoder
            |> Http.send
                (Result.mapError toString
                    >> fromResult WebDataError (EventsWebData sportID)
                )


updateSportTableState : Model -> Table.State -> Model
updateSportTableState m newTableState =
    case m.page of
        PageSport p ->
            { m | page = PageSport newTableState }

        x ->
            crash <| "updateSportTableState: " ++ toString x


updateEventsWebData : Model -> Int -> List Data.Aping.Event -> ( Model, Cmd Msg )
updateEventsWebData m sportID events =
    { m
        | events = Data.Aping.insertEvents m.events (dictID events)
        , sportEvents = Dict.insert sportID (List.map .id events) m.sportEvents
    }
        ! []


updateMarketsPrices : Model -> Data.Prices.WebData -> ( Model, Cmd Msg )
updateMarketsPrices m webdata =
    case (parseRoute m.location) of
        RouteEvent eventID ->
            case webdata of
                Data.Prices.WebMarket _ hashCode ->
                    m
                        ! [ WebSocket.send
                                (websocketURLPrices m.location eventID)
                                hashCode
                          ]

                Data.Prices.WebEvent event hashCode ->
                    { m
                        | events = Dict.insert event.id event m.events
                    }
                        ! [ WebSocket.send
                                (websocketURLPrices m.location event.id)
                                hashCode
                          ]

                Data.Prices.WebSessionID sessionID hashCode ->
                    updatePricesSessionID m sessionID hashCode

        _ ->
            m ! []
