module View.SportsMenu exposing (view)

import Html
    exposing
        ( Html
        , Attribute
        , a
        , li
        , ul
        , span
        , text
        )
import Html.Attributes exposing (class, classList, href, style, attribute, colspan, href, id)
import Data.Aping exposing (Sport)
import Routing exposing (Route, parseRoute)
import App exposing (Model)
import Dict


-- VIEW


view : Model -> Html a
view ({ location, sports, events } as model) =
    let
        sportID =
            case parseRoute location of
                Routing.RouteFootball ->
                    1

                Routing.RouteSport sportID ->
                    sportID

                Routing.RouteEvent eventID ->
                    App.tryGetSportByEventID model eventID
                        |> Maybe.map .id
                        |> Maybe.withDefault 0

        sports_ =
            sports
                |> Dict.toList
                |> List.map Tuple.second
                |> List.sortBy (\{ market_count } -> market_count * (-1))

        xs =
            List.drop 6 sports_

        xs1 =
            sports_
                |> List.take 6
                |> List.map (linkSport sportID)

        vx =
            case List.partition (\{ id } -> id == sportID) xs of
                ( [], xs2 ) ->
                    xs1 ++ [ view2 xs2 ]

                ( sport :: _, xs2 ) ->
                    xs1
                        ++ [ linkSport sportID sport
                           , view2 xs2
                           ]
    in
        ul [ class "nav nav-tabs" ] vx


view2 : List Sport -> Html a
view2 xs =
    li
        [ class "dropdown" ]
        [ a [ class "dropdown-toggle", attribute "data-toggle" "dropdown", href "#" ]
            [ text "Другие рынки...    "
            , span [ class "caret" ]
                []
            ]
        , List.map (linkSport 0) xs
            |> ul [ class "dropdown-menu" ]
        ]


linkSport : Int -> Sport -> Html a
linkSport sportID { name, id } =
    let
        href_ =
            href <| "#sport/" ++ toString id

        text_ =
            text name

        isActive =
            sportID == id

        link_ =
            if isActive then
                a [ href_ ] [ text_ ]
            else
                text_
    in
        li
            [ classList [ ( "active", isActive ) ] ]
            [ a [ href_ ] [ link_ ] ]
