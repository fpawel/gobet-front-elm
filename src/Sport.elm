module Sport
    exposing
        ( Model
        , Msg
        , update
        , init
        , viewMenuCountries
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
import Aping.Events
import Html.Attributes exposing (class, style, colspan, href)
import Html.Events exposing (onClick)
import Help.Component exposing (mainMenuItem, spinner_text)
import Help.Utils exposing (compareInvert, monthNumber)
import Navbar


-- MODEL


type alias Model =
    { location : Location
    , sports : List Aping.Sport
    , sport : Aping.Sport
    , events : List Event
    , countryFilter : CountryFilter
    , error : Maybe String
    }


type alias CountryFilter =
    Maybe String


type Msg
    = ApplyCountryFilter CountryFilter
    | NewEvents (Result Http.Error (List Event))


init :
    { location : Location
    , sports : List Aping.Sport
    , sport : Aping.Sport
    }
    -> ( Model, Cmd Msg )
init { location, sport, sports } =
    { location = location
    , sport = sport
    , sports = sports
    , events = []
    , countryFilter = Nothing
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
        ApplyCountryFilter cf ->
            { m | countryFilter = cf } ! []

        NewEvents (Ok events) ->
            { m | events = events } ! []

        NewEvents (Err error) ->
            { m | error = Just <| toString error }
                ! [ httpRequestEvents m.location m.sport ]



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


eventRow : Maybe String -> Event -> Html msg
eventRow countryFilter event =
    let
        day =
            Date.day event.openDate

        month =
            Date.month event.openDate

        year =
            Date.year event.openDate

        cellCountry =
            case countryFilter of
                Just _ ->
                    td [] []

                _ ->
                    td [] [ text event.country ]

        xs1 =
            [ td [] [ text <| toString day ]
            , td [] [ text <| toString month ]
            , td [] [ text <| toString year ]
            , cellCountry
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


linkCountryFilter : Maybe String -> Html Msg
linkCountryFilter countryFilter =
    li
        []
        [ Html.a
            [ href "#"
            , onClick (ApplyCountryFilter countryFilter)
            ]
            [ text <| countryFilterToString countryFilter ]
        ]


viewMenuCountries : (Msg -> msg) -> Model -> Html msg
viewMenuCountries toMsg ({ events, countryFilter } as m) =
    let
        countries =
            Aping.Events.countries events
    in
        if List.length countries < 2 then
            li [] []
        else
            countries
                |> List.map Just
                |> (::) Nothing
                |> List.map (linkCountryFilter >> Html.map toMsg)
                |> mainMenuItem (countryFilterToString countryFilter)


view : Model -> Html msg
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
    in
        [ { name = m.sport.name, items = dropNavSports } ]


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


getFilteredEvents : Model -> List Event
getFilteredEvents { sport, events, countryFilter } =
    events
        |> List.filter
            (\event ->
                case countryFilter of
                    Nothing ->
                        True

                    Just cf ->
                        event.country == cf
            )
        |> List.sortBy
            (\{ openDate } ->
                ( Date.year openDate
                , monthNumber <| Date.month <| openDate
                , Date.day openDate
                )
            )


viewEvents : Model -> Html msg
viewEvents ({ sport, countryFilter } as m) =
    let
        hr =
            tr []
                [ th [] [ text "День" ]
                , th [] [ text "Месяц" ]
                , th [] [ text "Год" ]
                , (case countryFilter of
                    Just _ ->
                        th [] []

                    _ ->
                        th [] [ text "Страна" ]
                  )
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
                , tbody [] <| List.map (eventRow countryFilter) filteredEvents
                ]
            , Html.p []
                [ h3 [] [ text <| toString sport.market_count ]
                , text "рынков"
                , h3 [] [ text <| toString <| List.length filteredEvents ]
                , text "событий"
                ]
            ]
