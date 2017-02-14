module Main exposing (..)


eventRow : Event -> Html a
eventRow { country, name } =
    case Aping.eventTeams name of
        Just ( home, away ) ->
            tr []
                [ td [] [ text country ]
                , td [] [ text home ]
                , td [] [ text away ]
                ]

        _ ->
            tr []
                [ td [] [ text country ]
                , td [ colspan 2 ] [ text name ]
                ]


dateRow : ( Int, Int, Int ) -> Html a
dateRow ( year, month, day ) =
    tr []
        [ th [ colspan 3 ]
            [ toString year
                ++ " "
                ++ toString day
                ++ " "
                ++ DateUtils.Month.format1 month
                |> text
            ]
        ]


eventsTable : List Event -> Html msg
eventsTable events =
    Aping.Events.groupByDays events
        |> Dict.toList
        |> List.sortBy Tuple.first
        |> List.map
            (\( k, v ) ->
                (dateRow k) :: (List.map eventRow v)
            )
        |> List.concat
        |> tbody []
        |> List.singleton
        |> table [ class "table table-condensed" ]
