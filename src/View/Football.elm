module View.Football exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import View.Help exposing (spinnerText)
import Data.Football exposing (Game)
import Dict exposing (Dict)
import Data.Aping exposing (Event)


viewGame : Dict Int Event -> Game -> Html a
viewGame events x =
    let
        td_ changed s =
            (if changed then
                td [ class "changed animated zoomIn" ]
             else
                td []
            )
                [ text s ]

        odd changed y =
            let
                value =
                    Maybe.withDefault "" <| Maybe.map toString y
            in
                td_ changed value

        country =
            events
                |> Dict.get x.eventID
                |> Maybe.map .country
                |> Maybe.withDefault ""
    in
        [ td [] [ Html.text <| (toString (x.page + 1)) ++ "." ++ (toString (x.order + 1)) ]
        , td [] [ Html.text country ]
        , td [ class "home-team" ] [ linkEvent x.eventID x.home ]
        , td_ x.uresult x.result
        , td [ class "away-team" ] [ linkEvent x.eventID x.away ]
        , td_ x.utime x.time
        , odd x.uwin1 x.win1
        , odd x.uwin2 x.win2
        , odd x.udraw1 x.draw1
        , odd x.udraw2 x.draw2
        , odd x.ulose1 x.lose1
        , odd x.ulose2 x.lose2
        ]
            |> tr []


viewGamesList : Dict Int Event -> List Game -> Html a
viewGamesList events games =
    let
        trs =
            games
                |> List.sortBy (\{ page, order } -> ( page, order ))
                |> List.map (viewGame events)

        headrow =
            [ "п/п"
            , "Страна"
            , "Дома"
            , "Счёт"
            , "В гостях"
            , "Время"
            , "П1+"
            , "П1-"
            , "Н+"
            , "Н-"
            , "П2+"
            , "П2-"
            ]
                |> List.map (\x -> th [] [ Html.text x ])
                |> tr []
    in
        table
            [ class "table table-condensed table-football"
            ]
            [ thead [] [ headrow ]
            , tbody [] trs
            ]


view : Dict Int Event -> List Game -> Html a
view events games =
    case games of
        [] ->
            spinnerText "Подготовка данных..."

        _ ->
            viewGamesList events games


linkEvent : Int -> String -> Html msg
linkEvent eventID str =
    a
        [ href <| "#event/" ++ toString eventID
        ]
        [ text str ]
