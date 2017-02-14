module View.SportTable exposing (config)

import Table exposing (defaultCustomizations)
import Aping exposing (eventTeams)
import Date
import Html exposing (Html, text, a)
import Html.Attributes exposing (class, colspan, href)
import DateUtils


linkEvent : Int -> String -> Html msg
linkEvent eventID str =
    a
        [ href <| "#event/" ++ toString eventID
        ]
        [ text str ]


columnOpenDate : Table.Column Aping.Event msg
columnOpenDate =
    Table.customColumn
        { name = "Дата открытия"
        , viewData =
            (\{ openDate } ->
                DateUtils.formatDate1 openDate
            )
        , sorter =
            Table.increasingOrDecreasingBy (.openDate >> Date.toTime)
        }


columnTime : Table.Column Aping.Event msg
columnTime =
    Table.customColumn
        { name = "Время"
        , viewData =
            (\{ openDate, timezone } ->
                DateUtils.formatTime1 openDate ++ ", " ++ timezone
            )
        , sorter = Table.unsortable
        }


columnVenue : Table.Column Aping.Event msg
columnVenue =
    Table.customColumn
        { name = "-"
        , viewData = .venue
        , sorter = Table.unsortable
        }


columnCountry : Table.Column Aping.Event msg
columnCountry =
    Table.customColumn
        { name = "Страна"
        , viewData = (.country)
        , sorter =
            Table.increasingOrDecreasingBy
                (\{ country, openDate } ->
                    ( country, Date.toTime openDate )
                )
        }


columnHome : Table.Column Aping.Event msg
columnHome =
    Table.veryCustomColumn
        { name = "Событие"
        , sorter =
            Table.increasingOrDecreasingBy
                (\{ name } ->
                    Aping.eventTeams name
                        |> Maybe.map Tuple.first
                        |> Maybe.withDefault name
                )
        , viewData =
            (\event ->
                case eventTeams event.name of
                    Just ( s, _ ) ->
                        { children = [ linkEvent event.id s ]
                        , attributes = []
                        }

                    _ ->
                        { children = [ linkEvent event.id event.name ]
                        , attributes = [ colspan 2 ]
                        }
            )
        }


columnAway : Table.Column Aping.Event msg
columnAway =
    Table.veryCustomColumn
        { name = "В гостях"
        , sorter =
            Table.increasingOrDecreasingBy
                (\{ name } ->
                    eventTeams name
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault ""
                )
        , viewData =
            (\event ->
                case eventTeams event.name of
                    Just ( _, s ) ->
                        { children = [ linkEvent event.id s ]
                        , attributes = []
                        }

                    _ ->
                        { children = []
                        , attributes = []
                        }
            )
        }


config : (Table.State -> msg) -> Table.Config Aping.Event msg
config toMsg =
    Table.customConfig
        { toId = (.id >> toString)
        , toMsg = toMsg
        , columns =
            [ columnOpenDate
            , columnTime
            , columnCountry
            , columnHome
            , columnAway
            , columnVenue
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs =
                    [ class "table table-condensed" ]
            }
        }
