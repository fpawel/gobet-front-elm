module DateUtils.Filter exposing (..)

import DateUtils exposing (Filter, Date, FilterTag(..))
import DateUtils.Month


values : List Filter
values =
    [ Just Today
    , Just Tomorrow
    , Just ThisMonth
    , Just ThisYear
    , Nothing
    ]


format : Date -> Filter -> String
format { month, year } x =
    case x of
        Just Today ->
            "Сегодня"

        Just Tomorrow ->
            "Завтра"

        Just ThisMonth ->
            let
                s =
                    DateUtils.Month.format2 month
            in
                case String.uncons s of
                    Just ( c, str ) ->
                        (String.toUpper <| String.fromChar c) ++ str

                    _ ->
                        s

        Just ThisYear ->
            toString year

        _ ->
            "Все дни"
