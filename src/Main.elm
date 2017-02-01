port module Main exposing (..)

import Html exposing (Html, Attribute, button, ul, li, h1, h3, span, div, nav)


-- import Html.Attributes exposing (class, href, style, attribute)

import Navigation
import UrlParser exposing ((</>), parseHash)
import Aping exposing (Event)


-- import Help.Component exposing (mainMenuItem)

import Routing
import Content
import Msg exposing (Msg)


--import Navbar


main : Program (List Aping.Sport) Model Msg
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
    , sports : List Aping.Sport
    , location : Navigation.Location
    }


init : List Aping.Sport -> Navigation.Location -> ( Model, Cmd Msg )
init sports location =
    let
        ( content, cmd ) =
            Content.init
                { location = location
                , sports = sports
                }
                (Routing.Sport 1)
    in
        Model content sports location ! [ cmd ]



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
                    Content.init
                        { location = model.location
                        , sports = model.sports
                        }
                        new_route
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


view : Model -> Html Msg
view model =
    Content.view model.content
