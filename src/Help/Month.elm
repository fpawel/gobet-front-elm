module Month exposing (..)

import Date exposing (Month(..))
import Dict exposing (Dict)
import Debug exposing (crash)


values : List Month
values =
    [ Jan
    , Feb
    , Mar
    , Apr
    , May
    , Jun
    , Jul
    , Aug
    , Sep
    , Oct
    , Nov
    , Dec
    ]


nvalues : List ( Int, ( String, Month ) )
nvalues =
    List.map3
        (,,)
        (List.range 1 12)
        [ "января"
        , "февраля"
        , "марта"
        , "апреля"
        , "мая"
        , "июня"
        , "июля"
        , "августа"
        , "сентября"
        , "октября"
        , "ноября"
        , "декабря"
        ]
        values
        |> List.map (\( a, b, c ) -> ( a, ( b, c ) ))


mvalues : Dict Int ( String, Month )
mvalues =
    Dict.fromList nvalues


fromNumber : Int -> Month
fromNumber n =
    Dict.get n mvalues
        |> getMaybe ("Month.fromNumber " ++ toString n)
        |> Tuple.second



--|> Maybe.withDefault (crash <| "Month.fromNumber " ++ toString n)


toNumber : Month -> Int
toNumber m =
    nvalues
        |> List.filterMap
            (\( n, ( _, m_ ) ) ->
                if m == m_ then
                    Just n
                else
                    Nothing
            )
        |> List.head
        |> getMaybe ("Month.toNumber " ++ toString m)


format1 : Int -> String
format1 n =
    Dict.get n mvalues
        |> Maybe.map (\( s, _ ) -> s)
        |> getMaybe ("Month.format1 " ++ toString n)


getMaybe : String -> Maybe a -> a
getMaybe s x =
    case x of
        Just y ->
            y

        _ ->
            Debug.crash s
