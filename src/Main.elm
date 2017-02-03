port module Main exposing (..)

import Html exposing (Html)
import Time exposing (Time)
import Navigation
import UrlParser exposing ((</>), parseHash)
import Aping exposing (Event)
import Routing
import Content
import Msg exposing (Msg)
import View.Container


main : Program { sports : List Aping.Sport, time : Time } Model Msg
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
    , time : Time
    }


init :
    { a | sports : List Aping.Sport, time : Time }
    -> Navigation.Location
    -> ( Model, Cmd Msg )
init { sports, time } location =
    { content = Content.football location
    , sports = sports
    , location = location
    , time = time
    }
        ! []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.Tick time ->
            { model | time = time } ! []

        Msg.UrlChange url ->
            let
                currentRoute =
                    Content.route model.content

                newRoute =
                    parseHash Routing.parser url
                        |> Maybe.withDefault currentRoute
            in
                if newRoute == currentRoute then
                    model ! []
                else
                    navigate newRoute model

        msg ->
            let
                ( content, cmd ) =
                    Content.update msg model.content
            in
                { model | content = content } ! [ cmd ]


navigate : Routing.Route -> Model -> ( Model, Cmd Msg )
navigate newRoute model =
    let
        ( content, cmd ) =
            case newRoute of
                Routing.Football ->
                    Content.football model.location ! []

                Routing.Sport sportID ->
                    Content.sport
                        { location = model.location
                        , sport = Aping.getSportByID sportID model.sports
                        , time = model.time
                        }
    in
        { model | content = content } ! [ cmd ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { content } =
    [ Content.subscriptions content
    , Time.every Time.second Msg.Tick
    ]
        |> Sub.batch



-- VIEW
-- "☰"


view : Model -> Html Msg
view { content, sports } =
    View.Container.view
        [ { name = "Футбол"
          , active = Content.route content == Routing.Football
          , route = "football"
          }
        , { name = "Обзор рынков"
          , active =
                case Content.route content of
                    Routing.Sport _ ->
                        True

                    _ ->
                        False
          , route = "sport/1"
          }
        ]
        []
        (Content.view sports content)
