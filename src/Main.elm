port module Main exposing (..)


import Html exposing (Html, Attribute, button, ul, li, h1, h3, span, div, nav)
import Html.Attributes exposing (class, href, style, attribute)
import Navigation

import Help.Component exposing (mainMenuItem)
import Football exposing (..)



main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Msg
    = MsgFootball Football.Msg
    | UrlChange Navigation.Location


type alias Model =
    { football : Football.Model
    }



init : Navigation.Location -> ( Model, Cmd Msg )
init location  =
    let
        ( mfb, cmd ) =
            Football.init location
    in
        { football = mfb } ! [ Cmd.map MsgFootball cmd ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        UrlChange url ->
          m ! []
        MsgFootball msgfb ->
            let
                ( mfb, cmdfb ) =
                    Football.update msgfb m.football
            in
                { m | football = mfb } ! [ Cmd.map MsgFootball cmdfb ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subFootball =
            Football.subscriptions model.football
    in
        Sub.batch
            [ Sub.map MsgFootball subFootball
            ]



-- VIEW
-- "☰"


view : Model -> Html Msg
view m =
    div
        []
        [ div
            [ class "container" ]
            [ Football.view (\_ -> "-") m.football
            ]
        ]
