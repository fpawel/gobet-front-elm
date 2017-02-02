module View.SportTable exposing (config)

import Table exposing (defaultCustomizations)
import Aping exposing (eventTeams)
import Date
import Html exposing (Html, text)
import Html.Attributes exposing (class, colspan)
import DateUtils exposing (FilterTag(..))


columnDayMonth : Table.Column { a | openDate : Date.Date } msg
columnDayMonth =
    Table.customColumn
        { name = "День"
        , viewData = (.openDate >> DateUtils.dateFromDate >> DateUtils.formatDate)
        , sorter = Table.increasingOrDecreasingBy (.openDate >> Date.toTime)
        }


columnYear : Table.Column { a | openDate : Date.Date } msg
columnYear =
    Table.customColumn
        { name = "Год"
        , viewData = (.openDate >> Date.year >> toString)
        , sorter =
            Table.increasingOrDecreasingBy
                (\{ openDate } ->
                    ( Date.year openDate, Date.toTime openDate )
                )
        }


columnCountry : Table.Column { a | openDate : Date.Date, country : String } msg
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


columnHome : Table.Column { a | name : String } msg
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
            (\{ name } ->
                case eventTeams name of
                    Just ( s, _ ) ->
                        { children = [ text s ]
                        , attributes = []
                        }

                    _ ->
                        { children = [ text name ]
                        , attributes = [ colspan 2 ]
                        }
            )
        }


columnAway : Table.Column { a | name : String } msg
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
            (\{ name } ->
                case eventTeams name of
                    Just ( _, s ) ->
                        { children = [ text s ]
                        , attributes = []
                        }

                    _ ->
                        { children = []
                        , attributes = []
                        }
            )
        }


filterColumns :
    Maybe FilterTag
    -> List (Table.Column Aping.Event msg)
filterColumns f =
    case f of
        Just Today ->
            [ columnCountry
            , columnHome
            , columnAway
            ]

        Just Tomorrow ->
            [ columnCountry
            , columnHome
            , columnAway
            ]

        Just ThisMonth ->
            [ columnDayMonth
            , columnCountry
            , columnHome
            , columnAway
            ]

        Just ThisYear ->
            [ columnDayMonth
            , columnCountry
            , columnHome
            , columnAway
            ]

        _ ->
            [ columnDayMonth
            , columnYear
            , columnCountry
            , columnHome
            , columnAway
            ]


config : (Table.State -> msg) -> DateUtils.Filter -> Table.Config Aping.Event msg
config toMsg filter =
    Table.customConfig
        { toId = (.id >> toString)
        , toMsg = toMsg
        , columns = filterColumns filter
        , customizations =
            { defaultCustomizations
                | tableAttrs =
                    [ class "table" ]
            }
        }
