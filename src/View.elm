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
import Dict
import Set exposing (Set)


view : Model -> Html Msg
view m =
    div
        []
        [ navbar (configNav m) (userButton m)
        , div
            [ class "container" ]
            [ page m ]
        ]


page : Model -> Html Msg
page ({ location, page, footballGames, events } as m) =
    case ( parseRoute location, page ) of
        ( RouteFootball, PageFootball ) ->
            let
                footballGamesIDs =
                    footballGames
                        |> List.map .eventID
                        |> Set.fromList

                footbalEvents =
                    events
                        |> Dict.filter
                            (\eventID event ->
                                Set.member eventID footballGamesIDs
                            )
            in
                div []
                    [ View.SportsMenu.view m
                    , View.Football.view footbalEvents footballGames
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


configNav : Model -> ConfigNav Msg
configNav { location } =
    let
        route =
            parseRoute location
    in
        [ { content =
                a
                    [ href "#football"
                    , style
                        [ ( "padding", "0px" )
                        , ( "border-color", "#2e6da4" )
                        ]
                    ]
                    [ div
                        [ class "my-tooltip-bottom hover-blue"
                        , style
                            [ ( "padding", "5px" )
                            ]
                        ]
                        [ img
                            [ src "icons/football-ball.svg"
                            , width 40
                            , height 40
                            ]
                            []
                        , span
                            [ class "my-tooltiptext-bottom" ]
                            [ text "Футбол сегодня" ]
                        ]
                    ]
          , active =
                route == Routing.RouteFootball
          }
        ]


userButton : Model -> Html Msg
userButton { location } =
    let
        route =
            parseRoute location
    in
        a
            [ class "my-tooltip-bottom hover-blue"
            , style
                [ ( "padding", "0px" )
                , ( "margin-left", "3px" )
                , ( "margin-right", "3px" )
                ]
            ]
            [ div
                [ class "my-tooltip-bottom"
                , style
                    [ ( "padding", "5px" )
                    ]
                ]
                [ img
                    [ src "icons/user.svg"
                    , width 40
                    , height 40
                    ]
                    []
                , span
                    [ class "my-tooltiptext-bottom" ]
                    [ text "Логин и пароль betfair.com" ]
                ]
            ]
