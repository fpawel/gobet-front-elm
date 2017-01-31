module Navbar exposing (Config, view, dropNav)

import Html exposing (Html, Attribute, button, ul, li, h1, h3, span, div, nav)
import Html.Attributes exposing (class, href, style, attribute)


type alias Config =
    List { name : String, items : List { name : String, route : String } }


navbarHeader : Html a
navbarHeader =
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


view : Config -> Html a
view menu =
    let
        --dropDownSports = dropDown { name = sport.name, items = dropNavSports sports }
        menu_element =
            menu
                |> List.map
                    (\{ name, items } -> dropDown { name = name, items = dropNav items })
                |> ul [ class "nav navbar-nav" ]
    in
        nav [ class "navbar navbar-default" ]
            [ div [ class "container-fluid" ]
                [ navbarHeader
                , div
                    [ class "collapse navbar-collapse" ]
                    [ menu_element
                    ]
                ]
            ]


dropNav : List { name : String, route : String } -> List (Html msg)
dropNav =
    List.map
        (\{ route, name } ->
            li []
                [ Html.a
                    [ href ("#" ++ route) ]
                    [ Html.text name ]
                ]
        )


dropDown : { name : String, items : List (Html msg) } -> Html msg
dropDown { name, items } =
    li
        [ class "dropdown" ]
        [ Html.a
            [ class "dropdown-toggle"
            , attribute "data-toggle" "dropdown"
            , href "#"
            ]
            [ Html.text name
            , span [ class "caret" ] []
            ]
        , ul
            [ class "dropdown-menu" ]
            items
        ]
