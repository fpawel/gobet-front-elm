module Help.Component exposing (..)

import Html.Attributes exposing (class, href, style, attribute)
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
        )


spinner_text : String -> Html msg
spinner_text text =
    div
        [ class "w3-display-container", style [ ( "height", "200px" ) ] ]
        [ div
            [ class "w3-display-left" ]
            [ Html.i
                [ class "fa fa-spinner fa-spin"
                , style
                    [ ( "font-size", "50px" )
                    , ( "margin", "5px" )
                    ]
                ]
                []
            ]
        , Html.text text
        ]


mainMenuItem : String -> List (Html msg) -> Html msg
mainMenuItem title items =
    li
        [ class "dropdown" ]
        [ Html.a
            [ class "dropdown-toggle"
            , attribute "data-toggle" "dropdown"
            , href "#"
            ]
            [ Html.text title
            , span [ class "caret" ] []
            ]
        , ul
            [ class "dropdown-menu" ]
            items
        ]
