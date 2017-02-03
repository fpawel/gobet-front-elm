module Main exposing (..)


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
