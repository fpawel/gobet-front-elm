module View.Container exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class, style, attribute, colspan, href, id)
import View.Navbar


view : View.Navbar.Config -> List (Html msg) -> Html msg
view navbarConfig content =
    div
        []
        [ View.Navbar.view navbarConfig
        , div
            [ class "container" ]
            content
        ]
