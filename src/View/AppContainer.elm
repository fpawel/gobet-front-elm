module View.AppContainer exposing (ConfigNav, ConfigDropNav, view)

import Html exposing (Html, Attribute, button, ul, li, h1, h3, span, div, nav)
import Html.Attributes exposing (class, classList, href, style, attribute)


type alias ConfigDropNav =
    List { name : String, items : ConfigNav }


type alias ConfigNav =
    List { name : String, route : String, active : Bool }


header : Html a
header =
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


navbar : ConfigNav -> ConfigDropNav -> Html a
navbar configNav configDropNav =
    let
        navline =
            liNav configNav

        dropnav =
            List.map
                (\{ name, items } -> dropDown { name = name, items = liNav items })
                configDropNav

        allnav =
            navline ++ dropnav
    in
        nav [ class "navbar navbar-default" ]
            [ div [ class "container-fluid" ]
                [ header
                , div
                    [ class "collapse navbar-collapse" ]
                    [ ul [ class "nav navbar-nav" ] allnav
                    ]
                ]
            ]


liNav : ConfigNav -> List (Html msg)
liNav =
    List.map
        (\{ route, name, active } ->
            li
                [ classList [ ( "active", active ) ]
                ]
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


view : ConfigNav -> ConfigDropNav -> List (Html msg) -> Html msg
view configNav configDropNav content =
    div
        []
        [ navbar configNav configDropNav
        , div
            [ class "container" ]
            content
        ]
