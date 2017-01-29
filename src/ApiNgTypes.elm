module ApiNgTypes exposing (EventType, Event, decoderEvent)

import Date
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import CountryCode


type alias EventType =
    { id : Int
    , name : String
    , market_count : Int
    }


type alias Event =
    { id : Int
    , name : String
    , country : String
    , openDate : Date.Date
    , timezone : String
    , venue : String
    }


getCountryNameByCode : String -> String
getCountryNameByCode =
    CountryCode.countryName
        >> Maybe.withDefault ""



-- DECODERS


decoderCountryName : Decoder String
decoderCountryName =
    D.map (CountryCode.countryName >> Maybe.withDefault "") D.string


decoderDate : Decoder Date.Date
decoderDate =
    D.map Date.fromString D.string
        |> D.andThen
            (\x ->
                case x of
                    Ok v ->
                        D.succeed v

                    Err e ->
                        D.fail e
            )


decoderEvent : Decoder Event
decoderEvent =
    decode Event
        |> required "id" D.int
        |> required "name" D.string
        |> optional "country_code" decoderCountryName ""
        |> required "open_date" decoderDate
        |> optional "time_zone" D.string ""
        |> optional "venue" D.string ""
