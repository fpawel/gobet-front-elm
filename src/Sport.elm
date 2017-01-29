module Sport exposing (Model, Msg, update, init, menuItem, view, eventType)

import Dict exposing (Dict)
import Regex exposing (..)
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
import ApiNgTypes
import Html.Attributes exposing (class, style, colspan, href)
import Html.Events exposing (onClick)
import Help.Component exposing (mainMenuItem, spinner_text)
import Help.Utils exposing (compareInvert, monthNumber)
import Date


-- MODEL


type Model
    = Model Context


type alias Context =
    { eventType : ApiNgTypes.EventType
    , events : List ApiNgTypes.Event
    , countryFilter : CountryFilter
    }


type alias CountryFilter =
    Maybe String


type Msg
    = ApplyCountryFilter CountryFilter


init : ApiNgTypes.EventType -> Model
init eventType =
    Model <| Context eventType [] Nothing


eventType : Model -> ApiNgTypes.EventType
eventType (Model { eventType }) =
    eventType



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model m) =
    case msg of
        ApplyCountryFilter cf ->
            Model { m | countryFilter = cf } ! []



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


eventRow : Maybe String -> ApiNgTypes.Event -> Html msg
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


getCountries : List ApiNgTypes.Event -> List String
getCountries =
    List.foldr
        (\event acc ->
            let
                n =
                    Dict.get event.country acc
                        |> Maybe.withDefault 0
            in
                Dict.insert event.country (n + 1) acc
        )
        Dict.empty
        >> Dict.toList
        >> List.sortWith
            (\( _, n1 ) ( _, n2 ) -> compareInvert n1 n2)
        >> List.map Tuple.first


menuItem : (Msg -> msg) -> Model -> Html msg
menuItem toMsg ((Model { events, countryFilter }) as m) =
    let
        countries =
            getCountries events
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
view (Model model) =
    let
        { eventType, events, countryFilter } =
            model

        hcountry =
            case countryFilter of
                Just _ ->
                    th [] []

                _ ->
                    th [] [ text "Страна" ]

        hr =
            tr []
                [ th [] [ text "День" ]
                , th [] [ text "Месяц" ]
                , th [] [ text "Год" ]
                , hcountry
                , th [ colspan 2 ] [ text "Событие" ]
                ]

        filteredEvents =
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
    in
        if List.isEmpty filteredEvents then
            spinner_text "Подготовка данных..."
        else
            div
                []
                [ table
                    [ class "table table-condensed" ]
                    [ thead [] [ hr ]
                    , tbody [] <| List.map (eventRow countryFilter) filteredEvents
                    ]
                , Html.p []
                    [ h3 [] [ text <| toString eventType.market_count ]
                    , text "рынков"
                    , h3 [] [ text <| toString <| List.length filteredEvents ]
                    , text "событий"
                    ]
                ]
