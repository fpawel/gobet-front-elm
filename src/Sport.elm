module Sport
    exposing
        ( Model
        , Msg
        , update
        , init
        , view
        )

import Http
import Regex exposing (..)
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
import Navigation exposing (Location)
import Aping exposing (Event)
import Aping.Decoder
import Html.Attributes exposing (class, style, colspan, href)
import Help.Component exposing (mainMenuItem, spinner_text)
import Month
import Table


-- MODEL


type alias Model =
    { location : Location
    , sport : Aping.Sport
    , events : List Event
    , tableState : Table.State
    , error : Maybe String
    }


type Msg
    = NewEvents (Result Http.Error (List Event))
    | SetTableState Table.State


init :
    { location : Location
    , sport : Aping.Sport
    }
    -> ( Model, Cmd Msg )
init { location, sport } =
    { location = location
    , sport = sport
    , events = []
    , tableState = Table.initialSort "Дата"
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



-- VIEW


viewEventName : String -> List (Html msg)
viewEventName s =
    case split (AtMost 1) (regex " [v@\\-] ") s of
        [ s1, s2 ] ->
            [ td [] [ text s1 ]
            , td [] [ text s2 ]
            ]

        _ ->
            [ td [ colspan 2 ] [ text s ] ]


config : Table.Config Aping.Event Msg
config =
    Table.config
        { toId = (.id >> toString)
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Дата" (.openDate >> formatDate)
            , Table.stringColumn "Страна" .country
            , Table.stringColumn "Событие" .name
            ]
        }


formatDate : Date.Date -> String
formatDate x =
    let
        day =
            Date.day x

        month =
            Date.month x

        year =
            Date.year x
    in
        toString day ++ " " ++ Month.format1 (Month.toNumber month) ++ " " ++ toString year


eventRow : Event -> Html msg
eventRow event =
    let
        day =
            Date.day event.openDate

        month =
            Date.month event.openDate

        year =
            Date.year event.openDate

        xs1 =
            [ td [] [ text <| toString day ++ " " ++ Month.format1 (Month.toNumber month) ++ " " ++ toString year ]
            , td [] [ text event.country ]
            ]
    in
        tr [] (xs1 ++ viewEventName event.name)


countryFilterToString : Maybe String -> String
countryFilterToString countryFilter =
    case countryFilter of
        Just "" ->
            "Международные события"

        Just s ->
            s

        _ ->
            "Все страны"


view : Model -> Html Msg
view { error, events, tableState } =
    case error of
        Nothing ->
            if List.isEmpty events then
                spinner_text "Подготовка данных..."
            else
                Table.view config tableState events

        Just error ->
            div []
                [ Html.p [] [ text error ]
                ]


getFilteredEvents : Model -> List Event
getFilteredEvents { sport, events } =
    events
        |> List.sortBy
            (\{ openDate } ->
                ( Date.year openDate
                , Month.toNumber <| Date.month <| openDate
                , Date.day openDate
                )
            )


viewEvents : Model -> Html msg
viewEvents ({ sport } as m) =
    let
        hr =
            tr []
                [ th [] [ text "Дата" ]
                , th [] [ text "Страна" ]
                , th [ colspan 2 ] [ text "Событие" ]
                ]

        filteredEvents =
            getFilteredEvents m
    in
        div
            []
            [ table
                [ class "table table-condensed" ]
                [ thead [] [ hr ]
                , tbody [] <| List.map eventRow filteredEvents
                ]
            , Html.p []
                [ h3 [] [ text <| toString sport.market_count ]
                , text "рынков"
                , h3 [] [ text <| toString <| List.length filteredEvents ]
                , text "событий"
                ]
            ]
