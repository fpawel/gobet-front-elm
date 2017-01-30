module Help.Utils exposing (..)

import Date exposing (Month(..))


compareInvert : comparable -> comparable -> Order
compareInvert n1 n2 =
    case compare n1 n2 of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


isJust : Maybe a -> Bool
isJust =
    Maybe.map (\_ -> True)
        >> Maybe.withDefault False


isNothing : Maybe a -> Bool
isNothing =
    Maybe.map (\_ -> False)
        >> Maybe.withDefault True


websocketURL : { a | host : String, protocol : String } -> String
websocketURL { protocol, host } =
    if not (String.startsWith "http" protocol) then
        "wrong protocol `"
            ++ protocol
            ++ "wrong name of the protocol - expected a string that starts with `http`"
            |> Debug.crash
    else
        "ws"
            ++ (String.dropLeft 4 protocol)
            ++ "//"
            ++ host


list_window1 : List (List a) -> Int -> List a -> List (List a)
list_window1 acc m xs =
    let
        ( xs1_, xs2_ ) =
            xs
                |> List.indexedMap (\n x -> ( n, x ))
                |> List.partition (\( n, _ ) -> n < m)

        xs1 =
            List.map Tuple.second xs1_

        xs2 =
            List.map Tuple.second xs2_
    in
        if List.isEmpty xs1 then
            acc
        else
            list_window1 (xs1 :: acc) m xs2


list_window : Int -> List a -> List (List a)
list_window n xs =
    list_window1 [] n xs
        |> List.reverse


list_parts_n : Int -> List a -> List (List a)
list_parts_n n xs =
    let
        len =
            List.length xs

        a =
            len // n

        b =
            rem len n

        c =
            if b == 0 then
                a
            else
                a + 1
    in
        list_window c xs


monthNumber : Month -> number
monthNumber x =
    case x of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
