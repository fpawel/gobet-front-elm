module Prices exposing (Market, decoderMarket)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)


type alias Market =
    { id : String
    , totalMatched : Maybe Float
    , totalAvailable : Maybe Float
    , runners : List Runner
    }


type alias Runner =
    { id : Int
    , odds : List Odd
    }


type alias Odd =
    { index : Int
    , side : String
    , odd : Maybe PriceSize
    }


type alias PriceSize =
    { price : Float
    , size : Float
    }


decoderMarket : Decoder Market
decoderMarket =
    decode Market
        |> required "id" D.string
        |> optional "totalMatched" (D.map Just D.float) Nothing
        |> optional "totalAvailable" (D.map Just D.float) Nothing
        |> optional "runners" (D.list runner) []


runner : Decoder Runner
runner =
    decode Runner
        |> required "id" D.int
        |> required "odds" (D.list odd)


odd : Decoder Odd
odd =
    decode Odd
        |> required "index" D.int
        |> required "side" D.string
        |> required "odd" (D.maybe priceSize)


priceSize : Decoder PriceSize
priceSize =
    decode PriceSize
        |> required "price" D.float
        |> required "size" D.float
