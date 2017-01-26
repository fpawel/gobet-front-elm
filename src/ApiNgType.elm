module ApiNgType exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)

type alias Event =
    { id : Int
    , name : String
    , countryCode : String
    , openDate : String
    , timezone : String
    , venue : String
    }


-- DECODERS


decoderEvent : Decoder Event
decoderEvent =
    decode Event
        |> required "id" D.int
        |> required "name" D.string
        |> optional "country_code" D.string ""
        |> optional "open_date" D.string ""
        |> optional "time_zone" D.string ""
        |> optional "venue" D.string ""
