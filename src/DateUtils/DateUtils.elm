module DateUtils exposing (..)

import Date exposing (Date)
import Time exposing (Time)
import DateUtils.Month


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


dateFromTime : Time -> Date
dateFromTime =
    Date.fromTime >> dateFromDate


timeIncDay : Float -> Time -> Time
timeIncDay n t =
    t + (n * 24 * Time.hour)


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


int2 : Int -> String
int2 n =
    (if n < 10 then
        "0"
     else
        ""
    )
        ++ toString n


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


formatTime1 : Date.Date -> String
formatTime1 d =
    let
        { minute, hour } =
            dateTimeFromDate d
    in
        (int2 hour)
            ++ ":"
            ++ (int2 minute)


formatDate1 : Date.Date -> String
formatDate1 d =
    let
        { day, month, year } =
            dateFromDate d
    in
        (int2 day) ++ " " ++ (int2 month) ++ " " ++ toString year


formatDayMonthYear : Date -> String
formatDayMonthYear ({ year } as x) =
    formatDayMonth x ++ " " ++ toString year


formatDayMonth : Date -> String
formatDayMonth { day, month } =
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


whatDayAfter : Time.Time -> Date -> String
whatDayAfter nowTime date =
    let
        nowDate =
            dateFromDate <| Date.fromTime nowTime

        isNthDayAfter n =
            date == { nowDate | day = nowDate.day + n }
    in
        if isNthDayAfter 0 then
            "Сегодня"
        else if isNthDayAfter 1 then
            "Завтра"
        else if isNthDayAfter 2 then
            "Послезавтра"
        else
            case List.filter isNthDayAfter (List.range 3 6) of
                n :: _ ->
                    Date.fromTime (nowTime + Time.hour * 24 * (toFloat n))
                        |> Date.dayOfWeek
                        |> toString

                _ ->
                    if date.year == nowDate.year then
                        formatDayMonth date
                    else
                        formatDayMonthYear date
