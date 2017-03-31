module View.Sport exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Data.Aping exposing (Event, eventTeams)
import App exposing (Model)
import View.Help exposing (spinnerText)
import Table exposing (defaultCustomizations)
import DateUtils
import Date
import Dict
import Routing
import Debug



-- VIEW


view : Model -> Html App.Msg
view ({ events, route, tableState } as model) =
    let
      sportID =
        case route of
          Routing.Sport sportID -> sportID
          _ -> Debug.crash <| "sport for " ++ toString route
    in
      thisEvents =
          


    if List.isEmpty events then
        spinnerText "Подготовка данных..."
    else
        Table.view
            (config App.SportTableState)
            tableState
            (Dict.get events)




linkEvent : Int -> String -> Html msg
linkEvent eventID str =
    a
        [ href <| "#event/" ++ toString eventID
        ]
        [ text str ]


columnOpenDate : Table.Column Event msg
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


columnTime : Table.Column Event msg
columnTime =
    Table.customColumn
        { name = "Время"
        , viewData =
            (\{ openDate, timezone } ->
                DateUtils.formatTime1 openDate ++ ", " ++ timezone
            )
        , sorter = Table.unsortable
        }


columnVenue : Table.Column Event msg
columnVenue =
    Table.customColumn
        { name = "-"
        , viewData = .venue
        , sorter = Table.unsortable
        }


columnCountry : Table.Column Event msg
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


columnHome : Table.Column Event msg
columnHome =
    Table.veryCustomColumn
        { name = "Событие"
        , sorter =
            Table.increasingOrDecreasingBy
                (\{ name } ->
                    eventTeams name
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


columnAway : Table.Column Event msg
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


config : (Table.State -> msg) -> Table.Config Event msg
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
