module Event exposing (..)

import Html exposing (Html, Attribute, text, span, div, table, td, tr, th, h3, h1, tbody)
import Html.Attributes as Attr exposing (class, colspan, rowspan, attribute, style)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import Json.Encode as E exposing (object, encode)
import Navigation exposing (Location)
import Aping
import Aping.Decoder
import Market
import DateUtils
import Help.Component exposing (spinner_text)
import WebSocket
import Help.Utils exposing (websocketURL)


-- MODEL


type alias Model =
    { eventID : Int
    , event : Maybe Aping.Event
    , markets : List Market.Model
    , location : Location
    , sessionID : String
    }


type Msg
    = MsgWebsocket (Result String WebsocketBatch)
    | MsgMarket String Market.Msg
    | NoOp String


type WebsocketData
    = WebsocketEvent Aping.Event
    | WebsocketMarket Aping.Market
    | WebsocketSessionID String


type alias WebsocketBatch =
    { data : WebsocketData
    , hashCode : String
    }


init : Location -> Int -> ( Model, Cmd Msg )
init location eventID =
    { eventID = eventID
    , event = Nothing
    , markets = []
    , location = location
    , sessionID = ""
    }
        ! []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        MsgMarket id msgMarket ->
            let
                ( markets, cmds_markets ) =
                    m.markets
                        |> List.map
                            (\mmarket ->
                                let
                                    ( mmarket_, cmd ) =
                                        if mmarket.market.id == id then
                                            Market.update msgMarket mmarket
                                        else
                                            mmarket ! []
                                in
                                    mmarket_ ! [ Cmd.map (MsgMarket mmarket.market.id) cmd ]
                            )
                        |> List.unzip

                cmds =
                    case msgMarket of
                        Market.ToggleCollapse marketID isExpanded ->
                            let
                                url =
                                    websocketURL m.location ++ "/wsprices-markets/" ++ m.sessionID

                                cmd =
                                    [ ( "market_id", E.string marketID )
                                    , ( "include", E.bool isExpanded )
                                    ]
                                        |> object
                                        |> encode 0
                                        |> WebSocket.send url
                            in
                                cmd :: cmds_markets
            in
                { m | markets = markets } ! cmds

        MsgWebsocket (Ok { data, hashCode }) ->
            case data of
                WebsocketEvent event ->
                    let
                        ( markets_, cmds ) =
                            event.markets
                                |> chooseMarkets
                                |> List.map
                                    (\market ->
                                        let
                                            ( mmarket_, cmd ) =
                                                Market.init m.location market
                                        in
                                            mmarket_ ! [ Cmd.map (MsgMarket market.id) cmd ]
                                    )
                                |> List.unzip
                    in
                        { m | event = Just event, markets = markets_ }
                            ! ((answerHashcode hashCode m) :: cmds)

                WebsocketMarket market ->
                    m ! [ answerHashcode hashCode m ]

                WebsocketSessionID sessionID ->
                    { m | sessionID = sessionID } ! [ answerHashcode hashCode m ]

        MsgWebsocket (Err error) ->
            let
                _ =
                    Debug.log ("EVENT " ++ toString m.eventID ++ " error") error
            in
                m ! []

        NoOp _ ->
            m ! []


answerHashcode : String -> Model -> Cmd Msg
answerHashcode hashCode m =
    WebSocket.send
        (websocketURLPrices m.location m.eventID)
        hashCode


chooseMarkets : List Aping.Market -> List Aping.Market
chooseMarkets =
    List.filter (\{ name } -> name /= "Азиатский гандикап")
        >> List.sortBy
            (\{ name, totalMatched } ->
                ( case name of
                    "Ставки" ->
                        0

                    "Результат" ->
                        1

                    _ ->
                        2
                , (-1) * totalMatched
                , name
                )
            )



-- VIEW


view1 : Aping.Event -> List Market.Model -> Html Msg
view1 event markets =
    let
        date =
            DateUtils.formatDayMonthYear <| DateUtils.dateFromDate event.openDate

        header =
            div [ class "page-header" ]
                [ h1 [] [ text <| event.name ]
                ]

        date_country =
            div [ style [ ( "text-align", "right" ) ] ]
                [ text (event.country ++ ", " ++ date) ]

        ( ms1_, ms2_ ) =
            markets
                |> List.indexedMap
                    (\n mmarket ->
                        ( n % 2
                        , Market.view mmarket
                            |> Html.map (MsgMarket mmarket.market.id)
                        )
                    )
                |> List.partition (Tuple.first >> (==) 0)

        ms1 =
            List.map Tuple.second ms1_

        ms2 =
            List.map Tuple.second ms2_
    in
        div []
            [ header
            , date_country
            , div
                [ class "row"
                , style [ ( "margin-top", "5px" ) ]
                ]
                [ div [ class "col-sm-6" ] ms1
                , div [ class "col-sm-6" ] ms2
                ]
            ]


view : Model -> Html Msg
view { event, markets } =
    case event of
        Just event ->
            view1 event markets

        _ ->
            spinner_text "Подготовка данных..."



-- DECODE


decoderWebsocketEvent : Decoder WebsocketData
decoderWebsocketEvent =
    decode WebsocketEvent
        |> required "event" Aping.Decoder.event


decoderWebsocketMarket : Decoder WebsocketData
decoderWebsocketMarket =
    decode WebsocketMarket
        |> required "market" Aping.Decoder.market


decoderWebsocketSessionID : Decoder WebsocketData
decoderWebsocketSessionID =
    decode WebsocketSessionID
        |> required "session_id" D.string


decoderWebsocket : Decoder WebsocketBatch
decoderWebsocket =
    decode WebsocketBatch
        |> required "ok"
            (D.oneOf
                [ decoderWebsocketEvent
                , decoderWebsocketMarket
                , decoderWebsocketSessionID
                ]
            )
        |> required "hash_code" D.string



-- SUBSCRIPTINS


websocketURLPrices : Navigation.Location -> Int -> String
websocketURLPrices location eventID =
    Help.Utils.websocketURL location ++ "/wsprices/" ++ toString eventID


subscriptions : Model -> Sub Msg
subscriptions { eventID, location, sessionID } =
    let
        wsprices =
            WebSocket.listen
                (websocketURLPrices location eventID)
                (D.decodeString decoderWebsocket)
                |> Sub.map MsgWebsocket

        subs =
            if sessionID == "" then
                [ wsprices ]
            else
                [ wsprices
                , WebSocket.listen
                    (websocketURL location ++ "/wsprices-markets/" ++ sessionID)
                    NoOp
                ]
    in
        Sub.batch subs
