module EventType exposing (Model, Msg, Config, update, init, getEventTypeID, menuItem, view)

import Dict exposing (Dict)
import Regex exposing (..)
import String
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
import Html.Attributes exposing (class, style, colspan, href)
import Html.Events exposing (onClick)
import Help.Component exposing (mainMenuItem, spinner_text)
import Help.Utils exposing (compareInvert)


-- MODEL


type Model
    = Model
        { id : Int
        , countryFilter : CountryFilter
        }


type alias CountryFilter =
    Maybe String


type Msg
    = ApplyCountryFilter CountryFilter


type alias Config msg =
    { toMsg : Msg -> msg
    }


init : Int -> Model
init id =
    Model { id = id, countryFilter = Nothing }



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


eventRow : ApiNg.Model -> Model -> ApiNg.Event -> Html msg
eventRow aping (Model { id, countryFilter }) x =
    let
        d =
            x.openDate

        date =
            (toString d.day ++ "/" ++ toString d.month ++ "/" ++ toString d.year)

        cellCountry =
            case countryFilter of
                Just _ ->
                    td [] []

                _ ->
                    td [] [ text <| Maybe.withDefault "" <| ApiNg.tryGetCountryName aping id x.id ]

        xs1 =
            [ td [] [ text <| toString d.day ]
            , td [] [ text <| toString d.month ]
            , td [] [ text <| toString d.year ]
            , cellCountry
            ]
    in
        tr [] (xs1 ++ viewEventName x.name)


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
            , onClick (MsgSetCountryFilter countryFilter)
            ]
            [ text <| countryFilterToString countryFilter ]
        ]


getCountries : ApiNg.Model -> Model -> List String
getCountries aping (Model { id }) =
    ApiNg.getEventType aping id
        |> Maybe.map .events
        |> Maybe.withDefault []
        |> List.foldr
            (\x acc ->
                let
                    cn =
                        ApiNg.tryGetCountryName aping id x.id
                            |> Maybe.withDefault ""

                    n =
                        Dict.get cn acc
                            |> Maybe.withDefault 0
                in
                    Dict.insert cn (n + 1) acc
            )
            Dict.empty
        |> Dict.toList
        |> List.sortWith
            (\( _, n1 ) ( _, n2 ) -> compareInvert n1 n2)
        |> List.map Tuple.first


menuItem : Config msg -> Model -> Html msg
menuItem config ((Model { id, countryFilter }) as m) =
    let
        countries =
            getCountries config.aping m
    in
        if List.length countries < 2 then
            li [] []
        else
            countries
                |> List.map Just
                |> (::) Nothing
                |> List.map (linkCountryFilter >> Html.map config.toMsg)
                |> mainMenuItem (countryFilterToString countryFilter)


view : ApiNg.Model -> Model -> Html msg
view aping (Model m) =
    let
        hcountry =
            case m.countryFilter of
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

        eventType =
            ApiNg.getEventType aping m.id

        events =
            eventType
                |> Maybe.map .events
                |> Maybe.withDefault []
                |> List.filter
                    (\x ->
                        case m.countryFilter of
                            Nothing ->
                                True

                            Just cf ->
                                (ApiNg.tryGetCountryName aping m.id x.id
                                    |> Maybe.withDefault ""
                                )
                                    == cf
                    )
                |> List.sortBy
                    (\{ openDate } ->
                        ( openDate.year
                        , openDate.month
                        , openDate.day
                        )
                    )

        rws =
            events
                |> List.map (eventRow aping <| Model m)

        marketCount =
            eventType |> Maybe.map (.marketCount >> toString) |> Maybe.withDefault ""

        eventCount =
            List.length events |> toString
    in
        if List.isEmpty rws then
            spinner_text "Подготовка данных..."
        else
            div
                []
                [ table
                    [ class "table table-condensed" ]
                    [ thead [] [ hr ]
                    , tbody [] rws
                    ]
                , Html.p []
                    [ h3 [] [ text marketCount ]
                    , text "рынков"
                    , h3 [] [ text eventCount ]
                    , text "событий"
                    ]
                ]
