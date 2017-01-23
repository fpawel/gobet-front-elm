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

    , home : String
    , away : String

    , page : Int
    , order : Int
    , result : String
    , time : String

    , win1 : Maybe Float
    , win2 : Maybe Float
    , draw1 : Maybe Float
    , draw2 : Maybe Float
    , lose1 : Maybe Float
    , lose2 : Maybe Float

    , inplay : Bool
    , utime : Bool
    , uresult : Bool

    }


type alias MaybeMaybeFloat = Maybe(Maybe Float)

type alias GameCahnges =
    { eventID : Int
    , page : Maybe Int
    , order : Maybe Int
    , time : Maybe String
    , result : Maybe String
    , win1 : MaybeMaybeFloat
    , win2 : MaybeMaybeFloat
    , draw1 : MaybeMaybeFloat
    , draw2 : MaybeMaybeFloat
    , lose1 : MaybeMaybeFloat
    , lose2 : MaybeMaybeFloat
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
updateGame x { page, order, time, result, win1, win2, draw1, draw2, lose1, lose2 } =
    { x
        | page = Maybe.withDefault x.page page
        , order = Maybe.withDefault x.order order
        , time = Maybe.withDefault x.time time
        , result = Maybe.withDefault x.result result
        , win1 = Maybe.withDefault x.win1 win1
        , win2 = Maybe.withDefault x.win2 win2
        , draw1 = Maybe.withDefault x.draw1 draw1
        , draw2 = Maybe.withDefault x.draw2 draw2
        , lose1 = Maybe.withDefault x.lose1 lose1
        , lose2 = Maybe.withDefault x.lose2 lose2

        , utime = isJust time
        , uresult = isJust result
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





decoderGame : Decoder Game
decoderGame =
    decode Game
        |> required "event_id" D.int
        |> required "market_id" D.int
        |> required "home" D.string
        |> required "away" D.string
        |> required "page" D.int
        |> required "order" D.int
        |> optional "result" D.string ""
        |> required "time" D.string

        |> optional "win1" (D.maybe D.float) Nothing
        |> optional "win2" (D.maybe D.float) Nothing
        |> optional "draw1" (D.maybe D.float) Nothing
        |> optional "draw2" (D.maybe D.float) Nothing
        |> optional "lose1" (D.maybe D.float) Nothing
        |> optional "lose2" (D.maybe D.float) Nothing

        |> hardcoded True
        |> hardcoded False
        |> hardcoded False



decoderMaybeFloat : Decoder (Maybe (Maybe Float))
decoderMaybeFloat =
  (D.maybe (D.maybe D.float))


decoderGameCahnges : Decoder GameCahnges
decoderGameCahnges =
    decode GameCahnges
        |> required "event_id" D.int
        |> optional "page" (D.maybe D.int) Nothing
        |> optional "order" (D.maybe D.int) Nothing
        |> optional "time" (D.maybe D.string) Nothing
        |> optional "result" (D.maybe D.string) Nothing

        |> optional "win1" decoderMaybeFloat Nothing
        |> optional "win2" decoderMaybeFloat Nothing
        |> optional "draw1" decoderMaybeFloat Nothing
        |> optional "draw2" decoderMaybeFloat Nothing
        |> optional "lose1" decoderMaybeFloat Nothing
        |> optional "lose2" decoderMaybeFloat Nothing


decoderReplyFromServer : Decoder ReplyFromServer
decoderReplyFromServer =
    decode ReplyFromServer
        |> optional "inplay" (D.list decoderGame) []
        |> optional "outplay" (D.list D.int) []
        |> optional "game_changes" (D.list decoderGameCahnges) []
        |> optional "request_id" (D.list D.int) []


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
        , ( "request_id", E.list (List.map E.int x.id) )
        ]


encodeGame : Game -> E.Value
encodeGame x =
    let
        encodedOdds =
            [ ( "win1", x.win1 )
            , ( "win2", x.win2 )
            , ( "draw1", x.draw1 )
            , ( "draw2", x.draw2 )
            , ( "lose1", x.lose1 )
            , ( "lose2", x.lose2 )
            ]
                |> List.filterMap
                    (\( k, v ) ->
                        v |> Maybe.map (\v -> ( k, E.float v ))
                    )

    in
        [ ( "event_id", E.int x.eventID )
        , ( "page", E.int x.page )
        , ( "order", E.int x.order )
        , ( "time", E.string x.time )
        , ( "result", E.string x.result )
        ] ++ encodedOdds
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
            td [] [ Html.text <| Maybe.withDefault "" <| Maybe.map toString y ]

        countryStr =
            getCountryByEventID x.eventID
    in
        [ td [] [ Html.text <| (toString (x.page + 1)) ++ "." ++ (toString (x.order + 1)) ]
        , td [] [ Html.text countryStr ]
        , td [ Attr.class "home-team" ] [ Html.text x.home ]
        , td_ x.uresult x.result
        , td [ Attr.class "away-team" ] [ Html.text x.away ]
        , td_ x.utime x.time
        , odd x.win1
        , odd x.win2
        , odd x.draw1
        , odd x.draw2
        , odd x.lose1
        , odd x.lose2
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
