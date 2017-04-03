module View.Market exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Data.Aping exposing (Market)
import App exposing (Msg(..))


view : Market -> Html Msg
view market =
    let
        marketNameElement =
            viewMarketName market

        runners =
            if market.isExpanded then
                viewRunners market.runners
            else
                []
    in
        div
            [ class "panel panel-primary" ]
            (if market.isExpanded then
                marketNameElement :: (viewRunners market.runners)
             else
                [ marketNameElement ]
            )


viewMarketName : Market -> Html Msg
viewMarketName market =
    let
        marketName =
            td
                [ attribute "width" "100%" ]
                [ span [ class "caret" ] []
                , span [ style [ ( "margin-left", "5px" ) ] ] [ text market.name ]
                ]

        totalMatchedElement =
            text <| (toString <| round market.totalMatched) ++ "$"

        head1 =
            if market.totalMatched /= 0 then
                [ marketName
                , td
                    [ style [ ( "color", "yellow" ) ] ]
                    [ totalMatchedElement ]
                ]
            else
                [ marketName ]
    in
        div
            [ class <|
                "panel-heading "
                    ++ (if market.isExpanded then
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
                [ tbody [] [ tr [] head1 ] ]
            ]


viewRunners : List Data.Aping.Runner -> List (Html msg)
viewRunners =
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
