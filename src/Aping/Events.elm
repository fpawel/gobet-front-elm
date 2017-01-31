module Aping.Events exposing (countries)

import Dict exposing (Dict)
import Help.Utils exposing (compareInvert)
import Aping exposing (..)


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
