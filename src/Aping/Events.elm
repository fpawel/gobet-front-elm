module Aping.Events
    exposing
        ( countries
        , groupByDays
        , defaultDay
        , dateTree
        , filterByDate
        , initFilter
        )

import Dict exposing (Dict)
import Set exposing (Set)
import Date
import Help.Utils exposing (compareInvert, listGoupBy)
import Aping exposing (..)
import Time
import DateUtils exposing (FilterTag(..))
import DateUtils.Month
import DateUtils.Filter


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
    listGoupBy
        (\event ->
            let
                day =
                    Date.day event.openDate

                month =
                    DateUtils.Month.toNumber <| Date.month event.openDate

                year =
                    Date.year event.openDate
            in
                ( day, month, year )
        )


dateTree :
    List Event
    -> List ( Int, List ( Int, List ( Int, List Event ) ) )
dateTree events =
    let
        mdays =
            groupByDays events

        ndays =
            Dict.toList mdays
                |> List.map Tuple.first

        nyears =
            ndays |> List.map (\( _, _, x ) -> x) |> Set.fromList
    in
        nyears
            |> Set.toList
            |> List.map
                (\y ->
                    ndays
                        |> List.filter (\( _, _, y_ ) -> y_ == y)
                        |> List.map (\( _, m, _ ) -> m)
                        |> Set.fromList
                        |> Set.toList
                        |> List.map
                            (\m ->
                                ndays
                                    |> List.filter
                                        (\( _, m_, y_ ) ->
                                            y_ == y && m_ == m
                                        )
                                    |> List.map (\( d, _, _ ) -> d)
                                    |> Set.fromList
                                    |> Set.toList
                                    |> List.map
                                        (\d ->
                                            Dict.get ( d, m, y ) mdays
                                                |> Maybe.withDefault []
                                                |> (,) d
                                        )
                                    |> (,) m
                            )
                        |> (,) y
                )


defaultDay : Day -> List Event -> Day
defaultDay today events =
    let
        days =
            groupByDays events
                |> Dict.toList
                |> List.map (Tuple.first)
    in
        if Set.member today <| Set.fromList days then
            today
        else
            days
                |> List.filter (\x -> x > today)
                |> List.head
                |> Maybe.withDefault
                    (days
                        |> List.head
                        |> Maybe.withDefault ( 0, 0, 0 )
                    )


initFilter : Time.Time -> List Event -> DateUtils.Filter
initFilter time events =
    DateUtils.Filter.values
        |> List.filterMap
            (\x ->
                case filterByDate time x events of
                    [] ->
                        Nothing

                    _ ->
                        Just x
            )
        |> List.head
        |> Maybe.withDefault Nothing


filterByDate : Time.Time -> DateUtils.Filter -> List Event -> List Event
filterByDate time customDate events =
    let
        now_ =
            DateUtils.dateFromDate (Date.fromTime time)
    in
        events
            |> List.filter
                (\{ openDate } ->
                    let
                        date =
                            DateUtils.dateFromDate openDate
                    in
                        case customDate of
                            Just Today ->
                                now_ == date

                            Just Tomorrow ->
                                let
                                    tomtime =
                                        time + 24 * Time.hour

                                    tomdate =
                                        Date.fromTime tomtime
                                in
                                    date == DateUtils.dateFromDate tomdate

                            Just ThisMonth ->
                                ( date.month, date.year ) == ( now_.month, now_.year )

                            Just ThisYear ->
                                date.year == now_.year

                            _ ->
                                True
                )
