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
            { m | route = Routing.parse nextLocation } ! []

        FootballWebData webdata ->
            updateFootball webdata m

        WebDataError error ->
            logError error m

        SportsWebData sports ->
            { m | sports = sports } ! []

        EventsWebData sportID newEvents ->
            { m | events = Dict.insert sportID newEvents m.events } ! []

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
