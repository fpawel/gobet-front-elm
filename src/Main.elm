port module Main exposing (..)

import Football exposing (..)
import Html exposing (Html, Attribute, button, ul, li, h1, h3, span, div, nav, programWithFlags)
import Html.Attributes exposing (class, href, style, attribute)
import Html.Events exposing (onClick)
import Help.Component exposing (mainMenuItem)


main : Program AppInit Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Msg
    = MsgFootball Football.Msg



type alias Model =
    { football : Football.Model
    }


type alias AppInit =
    { location : { protocol : String, host : String }
    }


init : AppInit -> ( Model, Cmd Msg )
init { location } =
  let
    ( mfb, cmd ) = Football.init location
  in
    { football = mfb } ! [ Cmd.map MsgFootball cmd ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
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
