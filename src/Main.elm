port module Main exposing (..)

import Html exposing (Html, Attribute, button, ul, li, h1, h3, span, div, nav)
import Html.Attributes exposing (class, href, style, attribute)
import Navigation
import UrlParser exposing ((</>), parseHash)
import Aping exposing (Event)
import Help.Component exposing (mainMenuItem)
import Routing
import Content
import Msg exposing (Msg)
import Navbar


main : Program (List EventType) Model Msg
main =
    Navigation.programWithFlags Msg.UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { content : Content.Model
    , eventTypes : List EventType
    , location : Navigation.Location
    }


init : List EventType -> Navigation.Location -> ( Model, Cmd Msg )
init eventTypes location =
    let
        ( content, cmd ) =
            Content.init location eventTypes Routing.Football
    in
        Model content eventTypes location ! [ cmd ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.UrlChange url ->
            let
                current_route =
                    Content.route model.content

                new_route =
                    parseHash Routing.parser url
                        |> Maybe.withDefault current_route
            in
                if new_route == current_route then
                    model ! []
                else
                    Content.init model.location model.eventTypes new_route
                        |> updateContent model

        msg ->
            Content.update msg model.content
                |> updateContent model


updateContent : Model -> ( Content.Model, Cmd Msg ) -> ( Model, Cmd Msg )
updateContent model ( content, cmd ) =
    { model | content = content } ! [ cmd ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { content } =
    Content.subscriptions content



-- VIEW
-- "☰"


navbarConfig : Model -> Navbar.Config
navbarConfig m =
    { sports = m.eventTypes
    , sport =
        case m.content of
            Content.Football _ ->
                { id = 1, name = "Футбол сегодня", market_count = 0 }

            Content.Sport m ->
                Sport.eventType m
    , menu =
        case m.content of
            Content.Football _ ->
                []

            Content.Sport m ->
                Sport.eventType m

            List { name = String, items = List { name = String, path = String } }
    }


dropNavEventTypes : Model -> List (Html Msg)
dropNavEventTypes { eventTypes } =
    let
        linkEventType { id, name } =
            Html.a
                [ href ("#sport/" ++ toString id)
                ]
                [ Html.text name ]
    in
        eventTypes
            |> List.sortBy (\{ market_count } -> market_count * -1)
            |> List.map linkEventType
            |> List.map (\x -> li [] [ x ])


navbarHeader : Html msg
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
            [ Html.text "Centbet" ]
        ]


navbar : Model -> Html Msg
navbar m =
    nav
        [ class "navbar navbar-default" ]
        [ div
            [ class "container-fluid" ]
            [ navbarHeader
            , div
                [ class "collapse navbar-collapse" ]
                [ ul
                    [ class "nav navbar-nav" ]
                    [ mainMenuItem (Content.what m.content) (dropNavEventTypes m)
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div
        []
        [ navbar model
        , div
            [ class "container" ]
            [ Content.view model.content
            ]
        ]
