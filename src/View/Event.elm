module View.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Data.Aping
import Data.Prices
import App exposing (Model, Msg)
import DateUtils
import Dict
import View.Market
import View.Help exposing (spinnerText)


type alias Markets =
    Dict.Dict String Data.Prices.Market



-- VIEW


view : Model -> Int -> Markets -> Html Msg
view m eventID markets =
    Dict.get eventID m.events
        |> Maybe.map (viewEvent markets)
        |> Maybe.withDefault (spinnerText "Подготовка данных...")


viewEvent : Markets -> Data.Aping.Event -> Html App.Msg
viewEvent markets event =
    let
        date =
            DateUtils.formatDayMonthYear <| DateUtils.dateFromDate event.openDate

        navbarHeader =
            div [ class "page-header" ]
                [ h1 [] [ text <| event.name ]
                ]

        date_country =
            div [ style [ ( "text-align", "right" ) ] ]
                [ text (event.country ++ ", " ++ date) ]

        choosenMarkets =
            event.markets
                |> Data.Aping.chooseMarkets

        templateMarkets =
            if List.isEmpty choosenMarkets then
                [ spinnerText "Запрос рынков..." ]
            else
                split2ColumnsMarkets markets choosenMarkets
    in
        div []
            [ navbarHeader
            , date_country
            , div
                [ class "row"
                , style [ ( "margin-top", "5px" ) ]
                ]
                templateMarkets
            ]


split2ColumnsMarkets : Markets -> List Data.Aping.Market -> List (Html Msg)
split2ColumnsMarkets markets choosenMarkets =
    let
        ( ms1_, ms2_ ) =
            choosenMarkets
                |> List.indexedMap
                    (\n market ->
                        ( n % 2
                        , View.Market.view market (Dict.get market.id markets)
                        )
                    )
                |> List.partition (Tuple.first >> (==) 0)

        ms1 =
            List.map Tuple.second ms1_

        ms2 =
            List.map Tuple.second ms2_
    in
        [ div [ class "col-sm-6" ] ms1
        , div [ class "col-sm-6" ] ms2
        ]
