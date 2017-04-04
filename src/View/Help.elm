module View.Help exposing (..)

import Html.Attributes exposing (..)
import Html exposing (..)


spinnerText : String -> Html msg
spinnerText text =
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
