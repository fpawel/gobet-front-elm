module Actions exposing (update, subscriptions)

import Dict exposing (Dict)
import WebSocket
import Http
import Json.Decode
import Navigation exposing (Location)
import Data.Football
import Data.Aping
import Routing exposing (Route)
import Help.Utils exposing (websocketURL)
import App exposing (Msg(..), Model)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        LocationChanged nextLocation ->
            locationChanged nextLocation m

        FootballWebData webdata ->
            updateFootball webdata m

        WebDataError error ->
            logError error m

        SportsWebData sports ->
            { m | sports = dictID sports } ! []

        EventsWebData sportID events ->
            { m
                | events = Data.Aping.insertEvents m.events (dictID events)
                , sportEvents = Dict.insert sportID (List.map .id events) m.sportEvents
            }
                ! []

        EventWebData event ->
            { m
                | events = Dict.insert event.id event m.events
            }
                ! []

        ToggleMarket marketID ->
            { m
                | events =
                    Dict.map (\_ -> Data.Aping.toggleMarketExpanded marketID) m.events
            }
                ! []

        _ ->
            m ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
    case m.route of
        Routing.Football ->
            WebSocket.listen
                (websocketURL m.rootLocation ++ "/football")
                (Data.Football.parseWebData
                    >> fromResult WebDataError FootballWebData
                )

        _ ->
            Sub.none


dictID : List { a | id : comparable } -> Dict comparable { a | id : comparable }
dictID =
    List.map (\x -> ( x.id, x ))
        >> Dict.fromList


logError : String -> Model -> ( Model, Cmd msg )
logError error m =
    let
        _ =
            Debug.log "web data error: " error
    in
        m ! []


locationChanged : Location -> Model -> ( Model, Cmd Msg )
locationChanged loc m =
    let
        route =
            Routing.parse loc
    in
        if m.route == route then
            m ! []
        else
            case route of
                Routing.Sport sportID ->
                    m ! [ webRequestEvents sportID m.rootLocation ]

                _ ->
                    m ! []


updateFootball : Data.Football.WebData -> Model -> ( Model, Cmd Msg )
updateFootball webdata m =
    let
        nextGames =
            Data.Football.updateGamesList webdata.changes m.footballGames

        answer =
            WebSocket.send
                (websocketURL m.rootLocation ++ "/football")
                webdata.hashCode
    in
        { m | footballGames = nextGames } ! [ answer ]


fromResult : (a -> x) -> (b -> x) -> Result a b -> x
fromResult fa fb r =
    case r of
        Err a ->
            fa a

        Ok b ->
            fb b


webRequestEvents : Int -> Location -> Cmd Msg
webRequestEvents sportID location =
    let
        eventsURL =
            location.protocol ++ "//" ++ location.host ++ "/events/" ++ toString sportID

        decoder =
            Json.Decode.list Data.Aping.decoderEvent
                |> Json.Decode.field "ok"
    in
        Http.get eventsURL decoder
            |> Http.send
                (Result.mapError toString
                    >> fromResult WebDataError (EventsWebData sportID)
                )