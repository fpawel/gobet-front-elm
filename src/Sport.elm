module Sport
    exposing
        ( Model
        , Msg
        , update
        , init
        , view
        , subscriptions
        )

import Http
import Json.Decode
import Html
    exposing
        ( Html
        , Attribute
        , button
        , ul
        , li
        , h1
        , h3
        , span
        , div
        , nav
        , tbody
        , tr
        , td
        , th
        , thead
        , table
        , text
        )
import Html.Attributes exposing (class, classList, style, colspan, href)
import Html.Events exposing (onClick)
import Date
import Time exposing (Time)
import Navigation exposing (Location)
import Aping exposing (Event)
import Aping.Decoder
import Aping.Events
import Help.Component exposing (mainMenuItem, spinner_text)
import Table exposing (defaultCustomizations)
import View.SportTable
import DateUtils exposing (FilterTag(..))
import DateUtils.Filter


-- MODEL


type alias Model =
    { location : Location
    , sport : Aping.Sport
    , events : List Event
    , tableState : Table.State
    , time : Time
    , customDate : CustomDate
    , error : Maybe String
    }


type Msg
    = NewEvents (Result Http.Error (List Event))
    | SetTableState Table.State
    | Tick Time
    | NewCustomDate CustomDate


type alias CustomDate =
    DateUtils.Filter


init :
    { location : Location
    , sport : Aping.Sport
    , time : Time
    }
    -> ( Model, Cmd Msg )
init { location, sport, time } =
    { location = location
    , sport = sport
    , events = []
    , tableState = Table.initialSort "Дата"
    , time = time
    , customDate = Just DateUtils.Today
    , error = Nothing
    }
        ! [ httpRequestEvents location sport ]


httpRequestEvents :
    Location
    -> Aping.Sport
    -> Cmd Msg
httpRequestEvents location eventType =
    let
        eventsURL =
            location.protocol ++ "//" ++ location.host ++ "/events/" ++ toString eventType.id

        decoder =
            Json.Decode.list Aping.Decoder.event
                |> Json.Decode.field "result"
    in
        Http.get eventsURL decoder
            |> Http.send NewEvents



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NewCustomDate x ->
            { m | customDate = x } ! []

        NewEvents (Ok events) ->
            { m
                | events = events
                , customDate = Aping.Events.initFilter m.time events
            }
                ! []

        NewEvents (Err error) ->
            { m | error = Just <| toString error }
                ! [ httpRequestEvents m.location m.sport ]

        SetTableState newState ->
            { m | tableState = newState } ! []

        Tick time ->
            { m | time = time } ! []



-- VIEW


view : Model -> Html Msg
view ({ error, events, tableState, time, customDate } as model) =
    case error of
        Nothing ->
            if List.isEmpty events then
                spinner_text "Подготовка данных..."
            else
                let
                    eventsToShow =
                        Aping.Events.filterByDate time customDate events
                in
                    div []
                        [ dateFilterBar model
                        , Table.view
                            (View.SportTable.config SetTableState customDate)
                            tableState
                            eventsToShow
                        ]

        Just error ->
            div []
                [ Html.p [] [ text error ]
                ]


dateFilterPill : Model -> DateUtils.Filter -> Html Msg
dateFilterPill { customDate, time } x =
    let
        ({ month, year } as now_) =
            now time
    in
        li [ classList [ ( "active", x == customDate ) ] ]
            [ Html.a [ href "#", onClick (NewCustomDate x) ]
                [ text <| DateUtils.Filter.format now_ x
                ]
            ]


dateFilterBar : Model -> Html Msg
dateFilterBar model =
    DateUtils.Filter.values
        |> List.map (dateFilterPill model)
        |> ul
            [ class "nav nav-pills"
            , style [ ( "margin", "10px" ) ]
            ]



-- SUBSCRIPTINS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every Time.second Tick



-- HELP


now : Time.Time -> DateUtils.Date
now time =
    DateUtils.dateFromDate (Date.fromTime time)
