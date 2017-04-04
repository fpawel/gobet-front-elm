module View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import App exposing (..)
import View.Bootstrap exposing (navbar, ConfigNav)
import View.Football
import View.SportsMenu
import View.Sport
import View.Event
import Routing exposing (Route(..), parseRoute)
import Debug exposing (crash, log)


view : Model -> Html Msg
view m =
    div
        []
        [ navbar (configNav m) []
        , div
            [ class "container" ]
            [ page m ]
        ]


page : Model -> Html Msg
page ({ location, page, footballGames } as m) =
    case ( parseRoute location, page ) of
        ( RouteFootball, PageFootball ) ->
            div []
                [ View.SportsMenu.view m
                , View.Football.view footballGames
                ]

        ( RouteSport sportID, PageSport ts ) ->
            div []
                [ View.SportsMenu.view m
                , View.Sport.view m sportID ts
                ]

        ( RouteEvent eventID, PageEvent { marketsPrices } ) ->
            div []
                [ View.SportsMenu.view m
                , View.Event.view m eventID marketsPrices
                ]

        x ->
            crash ("bad route page: " ++ toString x)


configNav : Model -> ConfigNav
configNav { location } =
    let
        route =
            parseRoute location
    in
        [ { name = "Футбол"
          , active = route == Routing.RouteFootball
          , route = "football"
          }
        , { name = "Обзор рынков"
          , active =
                case route of
                    Routing.RouteSport _ ->
                        True

                    _ ->
                        False
          , route = "sport/1"
          }
        ]
