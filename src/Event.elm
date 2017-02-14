module Event exposing (..)

import Html exposing (Html, Attribute, text, span, div, table, td, tr, th, h3, h1, tbody)
import Html.Attributes as Attr exposing (class, colspan, rowspan, attribute, style)
import Http
import Json.Decode
import Navigation exposing (Location)
import Aping
import Aping.Decoder
import Market
import DateUtils
import Help.Component exposing (spinner_text)


-- MODEL


type alias Model =
    { eventID : Int
    , event : Maybe Aping.Event
    , markets : List Market.Model
    , location : Location
    }


type Msg
    = ApingEvent (Result String Aping.Event)
    | MsgMarket String Market.Msg


init : Location -> Int -> ( Model, Cmd Msg )
init location eventID =
    { eventID = eventID
    , event = Nothing
    , markets = []
    , location = location
    }
        ! [ httpRequestEvent location eventID ]


httpRequestEvent : Location -> Int -> Cmd Msg
httpRequestEvent location eventID =
    let
        eventURL =
            location.protocol ++ "//" ++ location.host ++ "/event/" ++ toString eventID

        decoder =
            Aping.Decoder.event
                |> Json.Decode.field "ok"
    in
        Http.get eventURL decoder
            |> Http.send (Result.mapError toString >> ApingEvent)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        MsgMarket id msgMarket ->
            let
                ( markets, cmds ) =
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
            in
                { m | markets = markets } ! cmds

        ApingEvent (Ok event) ->
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
                { m | event = Just event, markets = markets_ } ! cmds

        ApingEvent (Err error) ->
            let
                _ =
                    Debug.log ("EVENT " ++ toString m.eventID ++ " error") error
            in
                m ! []


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



-- SUBSCRIPTINS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
