dropNavDays : Model -> List { name : String, route : String }
dropNavDays { events, sport } =
    events
        |> Aping.Events.groupByDays
        |> Dict.toList
        |> List.map Tuple.first
        |> List.sortBy (\( day, month, year ) -> ( year, month, day ))
        |> List.map
            (\day ->
                { name = formatDay1 day
                , route = "sport/" ++ toString sport.id ++ "_day_" ++ formatDay2 day
                }
            )

dropNavDateButton : Html msg
dropNavDateButton =
    button
        [ class "btn btn-primary dropdown-toggle"
        , attribute "data-toggle" "dropdown"
        , attribute "type" "button"
        ]
        [ text "Dropdown"
        , span [ class "caret" ]
            []
        ]


type Menu msg
    = Node String (List (Menu msg))
    | Leaf String msg


dropDownSubmenu : Menu msg -> Html msg
dropDownSubmenu menuItem =
    case menuItem of
        Node name items ->
            li [ class "dropdown-submenu" ]
                [ a [ href "#" ]
                    [ text name ]
                , ul [ class "dropdown-menu" ]
                    (List.map dropDownSubmenu items)
                ]

        Leaf name msg ->
            a
                [ href "#"
                , onClick msg
                ]
                [ text name ]
                |> List.singleton
                |> li []


dropNavDate : Model -> Html Msg
dropNavDate =
    .events
        >> Aping.Events.dateTree
        >> List.map
            (\( year, ms ) ->
                Node
                    (toString year)
                    (ms
                        |> List.map
                            (\( m, ds ) ->
                                Node
                                    (toString m)
                                    (ds
                                        |> List.map
                                            (\( d, _ ) ->
                                                Leaf
                                                    (toString d)
                                                    (NewDay ( d, m, year ))
                                            )
                                    )
                            )
                    )
            )
        >> List.map dropDownSubmenu
        >> ul
            [ class "dropdown-menu multi-level"
            , attribute "role" "menu"
            , attribute "aria-labelledby" "dropdownMenu"
            ]
        >> List.singleton
        >> (::) dropNavDateButton
        >> div [ class "dropdown" ]