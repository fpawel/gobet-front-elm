module Aping.Decoder exposing (event, market)

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


type alias Runner1 =
    { id : Int
    , name : String
    }


runner : Decoder Runner1
runner =
    decode Runner1
        |> required "selectionId" D.int
        |> required "runnerName" D.string


market : Decoder Aping.Market
market =
    decode Aping.Market
        |> required "marketId" D.int
        |> required "marketName" D.string
        |> required "totalMatched" D.float
        |> optional "runners" (D.list runner) []
        |> optional "competition" D.string ""
