module Football
    exposing
        ( Model
        , Msg
        , init
        , update
        , subscriptions
        , view
        )

import Debug
import WebSocket
import Dict exposing (Dict)
import Set exposing (Set)
import Random
import Time
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import Json.Encode as E exposing (object)
import Html exposing (Html, Attribute, span, div, table, td, tr, th)
import Html.Attributes as Attr
import Help.Utils exposing (isJust)
import Help.Component exposing (spinner_text)
import Styles as CssA


-- MODEL


type Model
    = Model
        { games : List Game
        , path :
            { host : String, protocol : String }
            -- счётчик миллисекунд задержки отправки запроса серверу
        , counter : Int
        }


type alias Game =
    { eventID : Int
    , marketID : Int
    , page : Int
    , order : Int
    , result : String
    , startTime : String
    , odds : Odds
    , home : String
    , away : String
    , inplay : Bool
    , startTimeChanged : Bool
    , resultChanged : Bool
    , oddsChanged : Bool
    }


type alias Odds =
    { win1 : Maybe Float
    , win2 : Maybe Float
    , draw1 : Maybe Float
    , draw2 : Maybe Float
    , lose1 : Maybe Float
    , lose2 : Maybe Float
    }


type alias GameCahnges =
    { eventID : Int
    , page : Maybe Int
    , order : Maybe Int
    , startTime : Maybe String
    , result : Maybe String
    , odds : Maybe Odds
    }


type Msg
    = MsgReplyFromServer ReplyFromServer
    | MsgReplyToServer ReplyToServer
    | MsgTick Time.Time


type alias ReplyToServer =
    { games : List Game
    , id : List Int
    }


type alias ReplyFromServer =
    { inplay : List Game
    , outplay : List Int
    , changes : List GameCahnges
    , id : List Int
    }


init : { host : String, protocol : String } -> ( Model, Cmd Msg )
init path =
    Model { games = [], path = path, counter = 0 } ! []


msgReplyToServer : List Game -> Cmd Msg
msgReplyToServer games =
    let
        gen =
            Random.list 5 (Random.int 0 20)
                |> Random.map (\id -> { games = games, id = id })
    in
        Random.generate MsgReplyToServer gen


isEmptyReplyFromServer : ReplyFromServer -> Bool
isEmptyReplyFromServer x =
    case ( x.inplay, x.outplay, x.changes, x.id ) of
        ( [], [], [], _ :: _ ) ->
            True

        _ ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model m) =
    case msg of
        MsgTick _ ->
            case m.counter of
                0 ->
                    Model m ! []

                1 ->
                    Model { m | counter = 0 } ! [ msgReplyToServer m.games ]

                n ->
                    Model { m | counter = n - 1 } ! []

        MsgReplyToServer replyToServer ->
            Model m
                ! [ sendWebsocket
                        m.path
                        replyToServer
                  ]

        MsgReplyFromServer replyFromServer ->
            let
                counter =
                    if isEmptyReplyFromServer replyFromServer then
                        10
                    else
                        3

                games =
                    updateGamesList replyFromServer m.games
            in
                Model { m | counter = counter, games = games } ! []


updateGame : Game -> GameCahnges -> Game
updateGame x { page, order, startTime, result, odds } =
    { x
        | page = Maybe.withDefault x.page page
        , order = Maybe.withDefault x.order order
        , startTime = Maybe.withDefault x.startTime startTime
        , result = Maybe.withDefault x.result result
        , odds = Maybe.withDefault x.odds odds
        , oddsChanged = isJust odds
        , startTimeChanged = isJust startTime
        , resultChanged = isJust result
    }


updateGamesList : ReplyFromServer -> List Game -> List Game
updateGamesList { inplay, outplay, changes } games =
    let
        outplaySet =
            outplay
                |> Set.fromList

        inplaySet =
            Set.fromList (List.map (\x -> x.eventID) inplay)

        changesMap =
            changes
                |> List.map (\x -> ( x.eventID, x ))
                |> Dict.fromList

        isJust =
            Maybe.map (\_ -> True)
                >> Maybe.withDefault False

        play =
            games
                |> List.filter
                    (\{ eventID } ->
                        (not <| Set.member eventID outplaySet)
                            && (not <| Set.member eventID inplaySet)
                    )
                |> List.map
                    (\x ->
                        Dict.get x.eventID changesMap
                            |> Maybe.map (updateGame x)
                            |> Maybe.withDefault x
                    )
    in
        inplay ++ play


websocketURL : { a | host : String, protocol : String } -> String
websocketURL path =
    Help.Utils.websocketURL path ++ "/football"


sendWebsocket : { a | host : String, protocol : String } -> ReplyToServer -> Cmd msg
sendWebsocket path reply =
    (encodeReplyToServer reply)
        |> E.encode 4
        |> WebSocket.send (websocketURL path)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model { path }) =
    [ WebSocket.listen (websocketURL path) decodeReplyFromServer
    , Time.every Time.second MsgTick
    ]
        |> Sub.batch



-- DECODERS


decoderOdds : Decoder Odds
decoderOdds =
    decode Odds
        |> optional "win1" (D.maybe D.float) Nothing
        |> optional "win2" (D.maybe D.float) Nothing
        |> optional "draw1" (D.maybe D.float) Nothing
        |> optional "draw2" (D.maybe D.float) Nothing
        |> optional "lose1" (D.maybe D.float) Nothing
        |> optional "lose2" (D.maybe D.float) Nothing


decoderGame : Decoder Game
decoderGame =
    decode Game
        |> required "eventID" D.int
        |> required "marketID" D.int
        |> required "page" D.int
        |> required "order" D.int
        |> required "result" D.string
        |> required "startTime" D.string
        |> optional "odds" decoderOdds (Odds Nothing Nothing Nothing Nothing Nothing Nothing)
        |> required "home" D.string
        |> required "away" D.string
        |> hardcoded True
        |> hardcoded False
        |> hardcoded False
        |> hardcoded False


decoderGameCahnges : Decoder GameCahnges
decoderGameCahnges =
    decode GameCahnges
        |> required "eventID" D.int
        |> optional "page" (D.maybe D.int) Nothing
        |> optional "order" (D.maybe D.int) Nothing
        |> optional "startTime" (D.maybe D.string) Nothing
        |> optional "result" (D.maybe D.string) Nothing
        |> optional "odds" (D.maybe decoderOdds) Nothing


decoderReplyFromServer : Decoder ReplyFromServer
decoderReplyFromServer =
    decode ReplyFromServer
        |> optional "inplay" (D.list decoderGame) []
        |> optional "outplay" (D.list D.int) []
        |> optional "changes" (D.list decoderGameCahnges) []
        |> optional "id" (D.list D.int) []


decodeReplyFromServer : String -> Msg
decodeReplyFromServer str =
    case D.decodeString decoderReplyFromServer str of
        Ok x ->
            MsgReplyFromServer x

        Err error ->
            Debug.crash ("error decoding `Football` message : " ++ error)



-- ENCODE


encodeReplyToServer : ReplyToServer -> E.Value
encodeReplyToServer x =
    object
        [ ( "games", E.list (List.map encodeGame x.games) )
        , ( "id", E.list (List.map E.int x.id) )
        ]


encodeGame : Game -> E.Value
encodeGame x =
    let
        encodedOdds =
            [ ( "win1", x.odds.win1 )
            , ( "win2", x.odds.win2 )
            , ( "draw1", x.odds.draw1 )
            , ( "draw2", x.odds.draw2 )
            , ( "lose1", x.odds.lose1 )
            , ( "lose2", x.odds.lose2 )
            ]
                |> List.filterMap
                    (\( k, v ) ->
                        v |> Maybe.map (\v -> ( k, E.float v ))
                    )
                |> object
    in
        [ ( "eventID", E.int x.eventID )
        , ( "page", E.int x.page )
        , ( "order", E.int x.order )
        , ( "startTime", E.string x.startTime )
        , ( "result", E.string x.result )
        , ( "odds", encodedOdds )
        ]
            |> object



-- VIEW


viewGame : (Int -> String) -> Game -> Html a
viewGame getCountryByEventID x =
    let
        jello_2s_attrs f =
            if f then
                CssA.animated_jello_2s
            else
                []

        bounceInUp_2s_attrs f =
            if f then
                CssA.animated_bounceInUp_2s
            else
                []

        td_ anim s =
            td (jello_2s_attrs anim) [ Html.text s ]

        odd y =
            td_ x.oddsChanged <| Maybe.withDefault "" <| Maybe.map toString y

        countryStr =
            getCountryByEventID x.eventID
    in
        [ td [] [ Html.text <| (toString (x.page + 1)) ++ "." ++ (toString (x.order + 1)) ]
        , td [] [ Html.text countryStr ]
        , td [ Attr.class "home-team" ] [ Html.text x.home ]
        , td_ x.resultChanged x.result
        , td [ Attr.class "away-team" ] [ Html.text x.away ]
        , td_ x.startTimeChanged x.startTime
        , odd x.odds.win1
        , odd x.odds.win2
        , odd x.odds.draw1
        , odd x.odds.draw2
        , odd x.odds.lose1
        , odd x.odds.lose2
        ]
            |> tr (bounceInUp_2s_attrs x.inplay)


viewGamesList : (Int -> String) -> List Game -> Html a
viewGamesList getCountryByEventID games =
    let
        trs =
            games
                |> List.sortBy (\{ page, order } -> ( page, order ))
                |> List.map (viewGame getCountryByEventID)

        thead =
            [ "п/п"
            , "Страна"
            , "Дома"
            , "Счёт"
            , "В гостях"
            , "Время"
            , "П1+"
            , "П1-"
            , "Н+"
            , "Н-"
            , "П2+"
            , "П2-"
            ]
                |> List.map (\x -> th [] [ Html.text x ])
                |> tr []
    in
        table
            [ Attr.class "table table-condensed"
            ]
            [ Html.thead [] [ thead ]
            , Html.tbody [] trs
            ]


view : (Int -> String) -> Model -> Html a
view getCountryByEventID (Model { games }) =
    case games of
        [] ->
            spinner_text "Подготовка данных..."

        _ ->
            viewGamesList getCountryByEventID games
