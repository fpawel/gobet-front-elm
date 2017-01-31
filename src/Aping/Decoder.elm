module Aping.Decoder exposing (event)

import Date
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import Help.CountryCode
import Aping exposing (..)


-- DECODERS


countryName : Decoder String
countryName =
    D.map
        (\x ->
            Help.CountryCode.countryName x
                |> Maybe.withDefault x
        )
        D.string


date : Decoder Date.Date
date =
    D.map Date.fromString D.string
        |> D.andThen
            (\x ->
                case x of
                    Ok v ->
                        D.succeed v

                    Err e ->
                        D.fail e
            )


event : Decoder Event
event =
    decode Event
        |> required "id" D.int
        |> required "name" D.string
        |> optional "country_code" countryName ""
        |> required "open_date" date
        |> optional "time_zone" D.string ""
        |> optional "venue" D.string ""
