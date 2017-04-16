module View.Bootstrap exposing (ConfigNav, navbar, mainMenuItem)

import Html exposing (..)
import Html.Attributes exposing (..)


type alias ConfigNav msg =
    List
        { content : Html msg
        , active : Bool
        }


navbar : ConfigNav a -> Html a -> Html a
navbar configNav navRight =
    nav [ class "navbar navbar-default" ]
        [ table [ attribute "width" "100%" ]
            [ tbody []
                [ tr []
                    [ td [] [ navbarHead ]
                    , td [ attribute "width" "100%" ]
                        [ div
                            [ class "collapse navbar-collapse" ]
                            [ ul [ class "nav navbar-nav" ] (liNav configNav) ]
                        ]
                    , td [] [ navRight ]
                    ]
                ]
            ]
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


navbarHead : Html a
navbarHead =
    div
        [ class "navbar-header" ]
        [ button
            [ attribute "type" "button"
            , class "navbar-toggle"
            , attribute "data-toggle" "collapse"
            , attribute "data-target" "#main-navbar"
            ]
            [ span [ class "icon-bar" ] []
            , span [ class "icon-bar" ] []
            , span [ class "icon-bar" ] []
            ]
        , Html.a
            [ class "navbar-brand"
            , href "#"
            ]
            [ Html.text "GOBET" ]
        ]


liNav : ConfigNav msg -> List (Html msg)
liNav =
    List.map
        (\{ content, active } ->
            li
                [ classList [ ( "active", active ) ]
                ]
                [ content
                ]
        )



{-
   Html.a
       [ href ("#" ++ route) ]
       [ content ]
   -
-}
