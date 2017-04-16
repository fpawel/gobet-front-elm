module View.Market exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Data.Aping
import Data.Prices
import Dict
import App exposing (Msg(..), Markets)
import Help.Utils exposing (isJust)


view : Data.Aping.Market -> Markets -> Html Msg
view market markets =
    let
        headPan =
            headPanel market markets
    in
        div
            [ class "panel panel-primary"
            , style [ ( "margin-bottom", "5px" ) ]
            ]
            (case Dict.get market.id markets of
                Just appMarket ->
                    headPan :: (viewRunners appMarket market.runners)

                _ ->
                    [ headPan ]
            )


headPanel : Data.Aping.Market -> Markets -> Html Msg
headPanel market markets =
    let
        marketName =
            td
                [ attribute "width" "100%" ]
                [ span [ class "caret" ] []
                , span [ style [ ( "margin-left", "5px" ) ] ] [ text market.name ]
                ]

        appMarket =
            Dict.get market.id markets

        totalMatched =
            appMarket
                |> Maybe.andThen .totalMatched
                |> Maybe.withDefault market.totalMatched
                |> tdMoney "totalMatched" "yellow"

        totalAvailable =
            appMarket
                |> Maybe.andThen .totalAvailable
                |> Maybe.withDefault market.totalAvailable
                |> tdMoney "totalAvailable" "LightCyan"

        row =
            [ Just marketName
            , totalMatched
            , totalAvailable
            ]
                |> List.filterMap identity
    in
        div
            [ class <|
                "panel-heading "
                    ++ (if isJust appMarket then
                            "dropup"
                        else
                            "dropdown"
                       )
            , style [ ( "padding", "3px 5px" ) ]
            , onClick (ToggleMarket market.id)
            , style [ ( "cursor", "pointer" ) ]
            , attribute "width" "100%"
            ]
            [ table
                [ attribute "width" "100%" ]
                [ tbody [] [ tr [] row ] ]
            ]


viewRunners : App.Market -> List Data.Aping.Runner -> List (Html msg)
viewRunners market =
    List.map (viewRunner market)
        >> tbody []
        >> List.singleton
        >> table [ class "table table-condenced table-market-runners" ]
        >> List.singleton
        >> div
            [ class "panel-body"
            , style [ ( "padding", "10px" ) ]
            ]
        >> List.singleton


viewRunner : App.Market -> Data.Aping.Runner -> Html msg
viewRunner market runner =
    let
        odd side =
            Dict.get ( runner.id, side, 0 ) market.prices
                |> Maybe.map (tdPriceSize side)
                |> Maybe.withDefault
                    (td [ attribute "width" "80px" ] [])
    in
        tr
            []
            [ td
                [ class "runner" ]
                [ text runner.name ]
            , odd "BACK"
            , odd "LAY"
            ]


tdPriceSize : String -> Data.Prices.PriceSize -> Html msg
tdPriceSize side { price, size } =
    td
        [ attribute "width" "80px"
        , class <|
            "price "
                ++ (if side == "BACK" then
                        "back"
                    else
                        "lay"
                   )
        ]
        [ div [ attribute "width" "100%" ]
            [ div
                [ style [ ( "font-weight", "bolder" ) ] ]
                [ text <| toString price ]
            , div
                [ style
                    [ ( "font-style", "italic" )
                    , ( "font-size", "small" )
                    ]
                ]
                [ toString (round size)
                    ++ "$"
                    |> text
                ]
            ]
        ]


tdMoney : String -> String -> Float -> Maybe (Html msg)
tdMoney tooltip color value =
    if value == 0 then
        Nothing
    else
        td
            [ style
                [ ( "color", color ) ]
            ]
            [ div [ class "my-tooltip-bottom" ]
                [ span
                    [ style [ ( "margin-left", "10px" ) ] ]
                    [ text <| (toString <| round value) ++ "$" ]
                , span
                    [ class "my-tooltiptext-bottom" ]
                    [ text tooltip ]
                ]
            ]
            |> Just


width100perc : Attribute msg
width100perc =
    attribute "width" "100%"
