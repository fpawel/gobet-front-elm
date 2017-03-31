module View.Football exposing (view)

import Html exposing (Html, Attribute, span, div, table, td, tr, th, h3, a, text)
import Html.Attributes as Attr exposing (class, href)
import Help.Component exposing (spinner_text)
import Styles as CssA
import Data.Football exposing (Game)


viewGame : Game -> Html a
viewGame x =
    let
        jello_2s_attrs f =
            if f then
                CssA.animated_jello_2s
            else
                []

        bounceInUp_2s_attrs f =
            if f then
                CssA.animated_bounceInUp_2s
            else
                []

        td_ anim s =
            td (jello_2s_attrs anim) [ Html.text s ]

        odd y =
            td [] [ Html.text <| Maybe.withDefault "" <| Maybe.map toString y ]
    in
        [ td [] [ Html.text <| (toString (x.page + 1)) ++ "." ++ (toString (x.order + 1)) ]
        , td [] [ Html.text x.event.country ]
        , td [ Attr.class "home-team" ] [ linkEvent x.event.id x.home ]
        , td_ x.uresult x.result
        , td [ Attr.class "away-team" ] [ linkEvent x.event.id x.away ]
        , td_ x.utime x.time
        , odd x.win1
        , odd x.win2
        , odd x.draw1
        , odd x.draw2
        , odd x.lose1
        , odd x.lose2
        ]
            |> tr (bounceInUp_2s_attrs x.inplay)


viewGamesList : List Game -> Html a
viewGamesList games =
    let
        trs =
            games
                |> List.sortBy (\{ page, order } -> ( page, order ))
                |> List.map viewGame

        thead =
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
            [ Attr.class "table table-condensed table-football"
            ]
            [ Html.thead [] [ thead ]
            , Html.tbody [] trs
            ]


view : List Game -> Html a
view games =
    case games of
        [] ->
            spinner_text "Подготовка данных..."

        _ ->
            viewGamesList games


linkEvent : Int -> String -> Html msg
linkEvent eventID str =
    a
        [ href <| "#event/" ++ toString eventID
        ]
        [ text str ]
