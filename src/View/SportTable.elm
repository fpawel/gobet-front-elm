module View.SportTable exposing (config)

import Table exposing (defaultCustomizations)
import Aping exposing (eventTeams)
import Date
import Html exposing (Html, text)
import Html.Attributes exposing (class, colspan)
import DateUtils


columnOpenDate : Table.Column { a | openDate : Date.Date, country : String } msg
columnOpenDate =
    Table.customColumn
        { name = "Дата открытия"
        , viewData = (.openDate >> DateUtils.formatDate1)
        , sorter =
            Table.increasingOrDecreasingBy (.openDate >> Date.toTime)
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


config : (Table.State -> msg) -> Table.Config Aping.Event msg
config toMsg =
    Table.customConfig
        { toId = (.id >> toString)
        , toMsg = toMsg
        , columns =
            [ columnOpenDate
            , columnCountry
            , columnHome
            , columnAway
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs =
                    [ class "table table-condensed" ]
            }
        }
