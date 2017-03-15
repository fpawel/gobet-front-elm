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
import Sports as MSports


main : Program { sports : List Aping.Sport, time : Time } Model Msg
main =
    Navigation.programWithFlags Msg.OnLocationChanged
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { content : Content.Model
    , sports : MSports.Model
    , location : Navigation.Location
    }


init :
    { a | sports : List Aping.Sport }
    -> Navigation.Location
    -> ( Model, Cmd Msg )
init { sports } location =
    let
        ( msports, cmdSports ) =
            MSports.init location
    in
        { content = Content.football location
        , sports = msports
        , location = location
        }
            ! [ Cmd.map Msg.Sports cmdSports ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.OnLocationChanged url ->
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

        Msg.Sports msg ->
            let
                ( sports, cmdsports ) =
                    MSports.update msg model.sports
            in
                { model | sports = sports } ! [ Cmd.map Msg.Sports cmdsports ]

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
                        , sport = Aping.getSportByID sportID model.sports.sports
                        }

                Routing.Event eventID ->
                    Content.event model.location eventID
    in
        { model | content = content } ! [ cmd ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { content } =
    Content.subscriptions content



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
        [ MSports.view (Content.sportID content) sports
        , Content.view sports.sports content
        ]
