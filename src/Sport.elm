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
import Regex exposing (..)
import Json.Decode
import Html.Events exposing (onClick)
import Html
    exposing
        ( Html
        , Attribute
        , a
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
import Dict exposing (Dict)


-- import Set exposing (Set)

import Time exposing (Time)
import Navigation exposing (Location)
import Aping exposing (Event)
import Aping.Decoder
import Aping.Events
import Html.Attributes exposing (class, style, attribute, colspan, href, id)


--import Html.Events exposing (onClick)

import Help.Component exposing (mainMenuItem, spinner_text)
import Help.Utils exposing (compareInvert, day_month_year)
import Month
import Navbar


-- MODEL


type alias Model =
    { location : Location
    , sports : List Aping.Sport
    , sport : Aping.Sport
    , events : List Event
    , time : Time
    , day : Day
    , error : Maybe String
    }


type alias Country =
    Maybe String


type alias Day =
    ( Int, Int, Int )


type Msg
    = NewEvents (Result Http.Error (List Event))
    | Tick Time
    | NewDay Day


init :
    { location : Location
    , sports : List Aping.Sport
    , sport : Aping.Sport
    }
    -> ( Model, Cmd Msg )
init ({ location, sport, sports } as xinit) =
    { location = location
    , sport = sport
    , sports = sports
    , events = []
    , day = ( 0, 0, 0 )
    , error = Nothing
    , time = 0
    }
        ! [ httpRequestEvents xinit ]


httpRequestEvents : { a | location : Location, sport : Aping.Sport } -> Cmd Msg
httpRequestEvents { location, sport } =
    let
        eventsURL =
            location.protocol ++ "//" ++ location.host ++ "/events/" ++ toString sport.id

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
        NewDay day ->
            { m | day = day } ! []

        Tick time ->
            { m | time = time } ! []

        NewEvents (Ok events) ->
            { m
                | events = events
                , day = Aping.Events.defaultDay (today m) events
            }
                ! []

        NewEvents (Err error) ->
            { m | error = Just <| toString error }
                ! [ httpRequestEvents m ]


today : { a | time : Time } -> Day
today { time } =
    day_month_year <| Date.fromTime time



-- VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ Navbar.view <| navbarConfig model
        , div
            [ class "container" ]
            [ view2 model
            ]
        ]


navbarConfig : Model -> Navbar.Config
navbarConfig m =
    let
        dropNavSports =
            m.sports
                |> List.sortBy (\{ market_count } -> market_count * -1)
                |> List.map
                    (\x -> { name = x.name, route = "sport/" ++ toString x.id })

        sports =
            Just { name = m.sport.name, items = dropNavSports }
    in
        [ { name = m.sport.name, items = dropNavSports }
        ]


formatDay1 : Day -> String
formatDay1 ( day, month, year ) =
    toString day ++ " " ++ Month.format1 month ++ " " ++ toString year


formatDay2 : Day -> String
formatDay2 ( day, month, year ) =
    toString day ++ "_" ++ toString month ++ "_" ++ toString year


dropNavSports : List Aping.Sport -> List { name : String, route : String }
dropNavSports sports =
    sports
        |> List.sortBy (\{ market_count } -> market_count * -1)
        |> List.map
            (\x -> { name = x.name, route = "sport/" ++ toString x.id })


view2 : Model -> Html msg
view2 model =
    case model.error of
        Nothing ->
            if List.isEmpty model.events then
                spinner_text "Подготовка данных..."
            else
                viewEvents model

        Just error ->
            div []
                [ Html.p [] [ text error ]
                ]


viewEvents : Model -> Html msg
viewEvents ({ sport, events } as m) =
    let
        events_ =
            events
                |> List.sortBy
                    (\{ openDate } ->
                        Date.toTime openDate
                    )
    in
        div
            []
            [ table
                [ class "table table-condensed" ]
                [ tbody [] <| List.map eventRow events_
                ]
            , Html.p []
                [ h3 [] [ text <| toString sport.market_count ]
                , text "рынков"
                , h3 [] [ text <| toString <| List.length events_ ]
                , text "событий"
                ]
            ]


eventRow : Event -> Html msg
eventRow { country, name, openDate } =
    [ td [] [ text <| formatDay1 <| day_month_year openDate ] ]
        ++ (viewEventName name)
        ++ [ td [] [ text country ] ]
        |> tr []


viewEventName : String -> List (Html msg)
viewEventName s =
    case split (AtMost 1) (regex " [v@\\-] ") s of
        [ s1, s2 ] ->
            [ td [] [ text s1 ]
            , td [] [ text s2 ]
            ]

        _ ->
            [ td [ colspan 2 ] [ text s ] ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second Tick
