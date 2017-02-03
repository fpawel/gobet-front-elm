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
import Html.Attributes exposing (colspan, class)
import Date
import Dict
import Time exposing (Time)
import Navigation exposing (Location)
import Aping exposing (Event)
import Aping.Decoder
import Aping.Events
import Help.Component exposing (mainMenuItem, spinner_text)
import Table exposing (defaultCustomizations)
import View.SportTable
import DateUtils
import DateUtils.Month


-- MODEL


type alias Model =
    { location : Location
    , sport : Aping.Sport
    , events : List Event
    , tableState : Table.State
    , time : Time
    , error : Maybe String
    }


type Msg
    = NewEvents (Result Http.Error (List Event))
    | SetTableState Table.State
    | Tick Time


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
        NewEvents (Ok events) ->
            { m | events = events } ! []

        NewEvents (Err error) ->
            { m | error = Just <| toString error }
                ! [ httpRequestEvents m.location m.sport ]

        SetTableState newState ->
            { m | tableState = newState } ! []

        Tick time ->
            { m | time = time } ! []



-- VIEW


view : Model -> Html Msg
view ({ error, events, tableState, time } as model) =
    case error of
        Nothing ->
            if List.isEmpty events then
                spinner_text "Подготовка данных..."
            else
                Table.view
                    (View.SportTable.config SetTableState)
                    tableState
                    events

        Just error ->
            div []
                [ Html.p [] [ text error ]
                ]


eventRow : Event -> Html a
eventRow { country, name } =
    case Aping.eventTeams name of
        Just ( home, away ) ->
            tr []
                [ td [] [ text country ]
                , td [] [ text home ]
                , td [] [ text away ]
                ]

        _ ->
            tr []
                [ td [] [ text country ]
                , td [ colspan 2 ] [ text name ]
                ]


dateRow : ( Int, Int, Int ) -> Html a
dateRow ( year, month, day ) =
    tr []
        [ th [ colspan 3 ]
            [ toString year
                ++ " "
                ++ toString day
                ++ " "
                ++ DateUtils.Month.format1 month
                |> text
            ]
        ]


eventsTable : List Event -> Html msg
eventsTable events =
    Aping.Events.groupByDays events
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map
            (\( k, v ) ->
                (dateRow k) :: (List.map eventRow v)
            )
        |> List.concat
        |> tbody []
        |> List.singleton
        |> table [ class "table table-condensed" ]



-- SUBSCRIPTINS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every Time.second Tick



-- HELP


now : Time.Time -> DateUtils.Date
now time =
    DateUtils.dateFromDate (Date.fromTime time)
