module View.Market exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Data.Aping
import Data.Prices
import App exposing (Msg(..))
import Help.Utils exposing (isJust)


view : Data.Aping.Market -> Maybe Data.Prices.Market -> Html Msg
view market prices =
    let
        headPan =
            headPanel market prices
    in
        div
            [ class "panel panel-primary" ]
            (if isJust prices then
                headPan :: (viewRunners prices market.runners)
             else
                [ headPan ]
            )


headPanel : Data.Aping.Market -> Maybe Data.Prices.Market -> Html Msg
headPanel market prices =
    let
        marketName =
            td
                [ attribute "width" "100%" ]
                [ span [ class "caret" ] []
                , span [ style [ ( "margin-left", "5px" ) ] ] [ text market.name ]
                ]

        totalMatched =
            prices
                |> Maybe.andThen .totalMatched
                |> Maybe.withDefault market.totalMatched
                |> tdMoney "yellow"

        totalAvailable =
            prices
                |> Maybe.andThen .totalAvailable
                |> Maybe.withDefault market.totalAvailable
                |> tdMoney "green"

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
                    ++ (if isJust prices then
                            "dropup"
                        else
                            "dropdown"
                       )
            , onClick (ToggleMarket market.id)
            , style [ ( "cursor", "pointer" ) ]
            , attribute "width" "100%"
            ]
            [ table
                [ attribute "width" "100%" ]
                [ tbody [] [ tr [] row ] ]
            ]


viewRunners : Maybe Data.Prices.Market -> List Data.Aping.Runner -> List (Html msg)
viewRunners prices =
    List.map (viewRunner prices)
        >> tbody []
        >> List.singleton
        >> table []
        >> List.singleton
        >> div [ class "panel-body" ]
        >> List.singleton


viewRunner : Maybe Data.Prices.Market -> Data.Aping.Runner -> Html msg
viewRunner prices runner =
    let
        xs =
            prices
                |> Maybe.andThen
                    (.runners
                        >> List.filter (\{ id } -> id == runner.id)
                        >> List.head
                    )
                |> Maybe.map .odds
                |> Maybe.withDefault []

        odd side =
            xs
                |> List.filter (\x -> x.side == side)
                |> List.head
                |> Maybe.andThen .odd
                |> Maybe.map
                    (\{ price, size } ->
                        toString (round price)
                            ++ " "
                            ++ toString (round size)
                            ++ "$"
                            |> text
                            |> List.singleton
                    )
                |> Maybe.withDefault []
    in
        tr
            []
            [ td [ attribute "width" "100%" ] [ text runner.name ]
            , td [ attribute "width" "40px" ] (odd "BACK")
            , td [ attribute "width" "40px" ] (odd "LAY")
            ]


tdMoney : String -> Float -> Maybe (Html msg)
tdMoney color value =
    if value == 0 then
        Nothing
    else
        td
            [ style [ ( "color", color ) ] ]
            [ text <| (toString <| round value) ++ "$" ]
            |> Just
