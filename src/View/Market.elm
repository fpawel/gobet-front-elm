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
            Maybe.andThen .totalMatched prices
                |> tdMoney "yellow"

        totalAvailable =
            Maybe.andThen .totalAvailable prices
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
    List.map
        (\{ name, id } ->
            tr
                []
                [ td [] [ text name ]
                ]
        )
        >> tbody []
        >> List.singleton
        >> table []
        >> List.singleton
        >> div [ class "panel-body" ]
        >> List.singleton


tdMoney : String -> Maybe number -> Maybe (Html msg)
tdMoney color =
    Maybe.andThen
        (\value ->
            if value == 0 then
                Nothing
            else
                Just value
        )
        >> Maybe.map
            (\value ->
                td
                    [ style [ ( "color", color ) ] ]
                    [ text <| (toString <| round value) ++ "$" ]
            )
