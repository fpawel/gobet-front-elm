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
import Date
import Time exposing (Time)
import Navigation exposing (Location)
import Aping exposing (Event)
import Aping.Decoder
import Help.Component exposing (mainMenuItem, spinner_text)
import Table exposing (defaultCustomizations)
import View.SportTable
import DateUtils


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
                div []
                    [ Table.view
                        (View.SportTable.config SetTableState)
                        tableState
                        events
                    ]

        Just error ->
            div []
                [ Html.p [] [ text error ]
                ]



{--
dateFilterPill : Model -> FilterDay -> Html Msg
dateFilterPill { filterDay, time } x =
    let
        ({ month, year } as now_) =
            now time

        s =
            case x of
                Just a ->
                    DateUtils.whatDayAfter time a

                _ ->
                    "Все дни..."
    in
        li [ classList [ ( "active", x == filterDay ) ] ]
            [ Html.a [ href "#", onClick (NewFilterDay x) ]
                [ text s
                ]
            ]


dateFilterBar : Model -> Html Msg
dateFilterBar model =
    let
        stl =
            style [ ( "margin", "10px" ) ]

        xs =
            (Aping.Events.daysFilters model.time model.events
                |> List.map Just
            )
                ++ [ Nothing ]
    in
        if List.length model.events < 50 || xs == [] then
            div [ stl ] []
        else
            xs
                |> List.map (dateFilterPill model)
                |> ul
                    [ class "nav nav-pills"
                    , stl
                    ]

-}
-- SUBSCRIPTINS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every Time.second Tick



-- HELP


now : Time.Time -> DateUtils.Date
now time =
    DateUtils.dateFromDate (Date.fromTime time)
