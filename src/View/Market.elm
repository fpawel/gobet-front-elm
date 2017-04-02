module View.Market exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Data.Aping exposing (Market)
import App exposing (Msg(..))


view : Market -> Html Msg
view market =
    let
        runners =
            if market.isExpanded then
                market.runners
                    |> List.map
                        (\{ name, id } ->
                            tr
                                []
                                [ td [] [ text name ]
                                ]
                        )
                    |> tbody []
                    |> List.singleton
                    |> table []
                    |> List.singleton
                    |> div [ class "panel-body" ]
                    |> List.singleton
            else
                []

        marketName =
            td
                [ attribute "width" "100%" ]
                [ span [ class "caret" ] []
                , span [ style [ ( "margin-left", "5px" ) ] ] [ text market.name ]
                ]

        totalMatchedElement =
            round market.totalMatched

        head1 =
            if market.totalMatched /= 0 then
                [ marketName
                , td
                    [ style [ ( "color", "yellow" ) ]
                    ]
                    [ text <| (toString market.totalMatched) ++ "$"
                    ]
                ]
            else
                [ marketName ]

        heading =
            div
                [ class <|
                    "panel-heading "
                        ++ (if market.isExpanded then
                                "dropup"
                            else
                                "dropdown"
                           )
                , onClick (ToggleMarket market.id (not market.isExpanded))
                , style [ ( "cursor", "pointer" ) ]
                , attribute "width" "100%"
                ]
                [ table
                    [ attribute "width" "100%" ]
                    [ tbody [] [ tr [] head1 ] ]
                ]
    in
        div
            [ class "panel panel-primary" ]
            ([ heading ] ++ runners)
