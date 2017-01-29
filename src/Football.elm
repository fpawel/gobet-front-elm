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
import Navigation exposing (Location)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import Json.Encode as E exposing (object)
import Html exposing (Html, Attribute, span, div, table, td, tr, th)
import Html.Attributes as Attr
import Help.Utils exposing (isJust)
import Help.Component exposing (spinner_text)
import Styles as CssA
import ApiNgTypes exposing (Event, decoderEvent)


-- MODEL


type Model
    = Model
        { games : List Game
        , location : Location
        }


type alias Game =
    { eventID : Int
    , marketID : Int
    , home : String
    , away : String
    , event : Event
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


type alias MaybeMaybeFloat =
    Maybe (Maybe Float)


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


type alias ReplyFromServer =
    { inplay : List Game
    , outplay : List Int
    , changes : List GameCahnges
    , hashCode : String
    }


init : Location -> Model
init location =
    Model { games = [], location = location }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model m) =
    case msg of
        MsgReplyFromServer replyFromServer ->
            let
                games =
                    updateGamesList replyFromServer m.games

                answer =
                    WebSocket.send (websocketURL m.location) replyFromServer.hashCode
            in
                Model { m | games = games } ! [ answer ]


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model { location }) =
    [ WebSocket.listen (websocketURL location) decodeReplyFromServer
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
        |> required "event" decoderEvent
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
    decode
        ReplyFromServer
        |> optional "inplay" (D.list decoderGame) []
        |> optional "outplay" (D.list D.int) []
        |> optional "game_changes" (D.list decoderGameCahnges) []
        |> required "hash_code" D.string


decodeReplyFromServer : String -> Msg
decodeReplyFromServer str =
    case D.decodeString decoderReplyFromServer str of
        Ok x ->
            MsgReplyFromServer x

        Err error ->
            Debug.crash ("error decoding `Football` message : " ++ error)



-- ENCODE


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
        ]
            ++ encodedOdds
            |> object



-- VIEW


viewGame : Game -> Html a
viewGame x =
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
    in
        [ td [] [ Html.text <| (toString (x.page + 1)) ++ "." ++ (toString (x.order + 1)) ]
        , td [] [ Html.text x.event.country ]
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


viewGamesList : List Game -> Html a
viewGamesList games =
    let
        trs =
            games
                |> List.sortBy (\{ page, order } -> ( page, order ))
                |> List.map viewGame

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


view : Model -> Html a
view (Model { games }) =
    case games of
        [] ->
            spinner_text "Подготовка данных..."

        _ ->
            viewGamesList games
