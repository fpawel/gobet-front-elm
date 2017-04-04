port module Main exposing (..)

import Html exposing (Html)
import Navigation
import UrlParser exposing ((</>), parseHash)
import Data.Aping exposing (Event)
import Routing
import Content
import Msg exposing (Msg)
import View.Container
import SportsMenu


main : Program Never Model Msg
main =
    Navigation.program Msg.OnLocationChanged
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { content : Content.Model
    , sportsMenu : SportsMenu.Model
    , location : Navigation.Location
    }


init :
    Navigation.Location
    -> ( Model, Cmd Msg )
init location =
    let
        ( msports, cmdSports ) =
            SportsMenu.init location
    in
        { content = Content.football location
        , sportsMenu = msports
        , location = location
        }
            ! [ Cmd.map Msg.SportsMenu cmdSports ]



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

        Msg.SportsMenu msg ->
            let
                ( nextSportsMenu, cmdsports ) =
                    SportsMenu.update msg model.sportsMenu
            in
                { model | sportsMenu = nextSportsMenu } ! [ Cmd.map Msg.SportsMenu cmdsports ]

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
                Routing.RouteFootball ->
                    Content.football model.location ! []

                Routing.RouteSport sportID ->
                    Content.sport
                        { location = model.location
                        , sport = Aping.getSportByID sportID model.sportsMenu.sports
                        }

                Routing.RouteEvent eventID ->
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
view { content, sportsMenu } =
    View.Container.view
        [ { name = "Футбол"
          , active = Content.route content == Routing.RouteFootball
          , route = "football"
          }
        , { name = "Обзор рынков"
          , active =
                case Content.route content of
                    Routing.RouteSport _ ->
                        True

                    _ ->
                        False
          , route = "sport/1"
          }
        ]
        []
        [ SportsMenu.view (Content.sportID content) sportsMenu
        , Content.view sportsMenu.sports content
        ]
