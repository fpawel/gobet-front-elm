port module Main exposing (..)

import Html exposing (Html)
import Navigation
import UrlParser exposing ((</>), parseHash)
import Aping exposing (Event)
import Routing
import Content
import Msg exposing (Msg)
import View.Container


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
                , time = 0
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
                    let
                        time =
                            case model.content of
                                Content.Sport { time } ->
                                    time
                    in
                        Content.init
                            { location = model.location
                            , sports = model.sports
                            , time = time
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
    View.Container.view [] (Content.view model.sports model.content)
