module Aping.Events exposing (..)

import Dict exposing (Dict)
import Date
import Help.Utils exposing (compareInvert, listGroupBy)
import Aping exposing (..)
import DateUtils.Month


type alias Day =
    ( Int, Int, Int )


countries : List Event -> List String
countries =
    List.foldr
        (\event acc ->
            let
                n =
                    Dict.get event.country acc
                        |> Maybe.withDefault 0
            in
                Dict.insert event.country (n + 1) acc
        )
        Dict.empty
        >> Dict.toList
        >> List.sortWith
            (\( _, n1 ) ( _, n2 ) -> compareInvert n1 n2)
        >> List.map Tuple.first


groupByDays : List Event -> Dict Day (List Event)
groupByDays =
    listGroupBy
        (\event ->
            let
                day =
                    Date.day event.openDate

                month =
                    DateUtils.Month.toNumber <| Date.month event.openDate

                year =
                    Date.year event.openDate
            in
                ( year, month, day )
        )
