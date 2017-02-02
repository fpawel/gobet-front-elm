module DateUtils exposing (..)

import Date
import DateUtils.Month


type FilterTag
    = Today
    | Tomorrow
    | ThisMonth
    | ThisYear


type alias Filter =
    Maybe FilterTag


type alias Date =
    { day : Int
    , month : Int
    , year : Int
    }


type alias DateTime =
    { millis : Int
    , second : Int
    , minute : Int
    , hour : Int
    , day : Int
    , month : Int
    , year : Int
    }


dateFromDate : Date.Date -> Date
dateFromDate date =
    let
        day =
            Date.day date

        month =
            DateUtils.Month.toNumber <| Date.month date

        year =
            Date.year date
    in
        { day = day
        , month = month
        , year = year
        }


dateTimeFromDate : Date.Date -> DateTime
dateTimeFromDate date =
    let
        millis =
            Date.millisecond date

        second =
            Date.second date

        minute =
            Date.minute date

        hour =
            Date.hour date

        day =
            Date.day date

        month =
            DateUtils.Month.toNumber <| Date.month date

        year =
            Date.year date
    in
        { millis = millis
        , second = second
        , minute = minute
        , hour = hour
        , day = day
        , month = month
        , year = year
        }


formatDate : Date -> String
formatDate { day, month, year } =
    let
        strDay =
            (if day < 10 then
                "0"
             else
                ""
            )
                ++ toString day

        strMonth =
            DateUtils.Month.format1 month
    in
        strDay ++ " " ++ strMonth
