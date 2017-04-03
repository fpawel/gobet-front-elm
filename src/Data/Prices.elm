module Data.Prices exposing (WebData(..), Market, decodeWebData, encodeToggleMarket)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (object, encode)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import Data.Aping exposing (Event, decoderEvent)


type WebData
    = WebEvent Data.Aping.Event String
    | WebMarket Market String
    | WebSessionID String String


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


encodeToggleMarket :
    { a | id : String, include : Bool, session : String }
    -> String
encodeToggleMarket x =
    [ ( "market_id", E.string x.id )
    , ( "include", E.bool x.include )
    , ( "session_id", E.string x.session )
    ]
        |> object
        |> encode 0



-- DECODE


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


decoderWebsocketEvent : Decoder (String -> WebData)
decoderWebsocketEvent =
    decode WebEvent
        |> required "event" Data.Aping.decoderEvent


decoderWebsocketPricesMarket : Decoder (String -> WebData)
decoderWebsocketPricesMarket =
    decode WebMarket
        |> required "market" decoderMarket


decoderWebsocketSessionID : Decoder (String -> WebData)
decoderWebsocketSessionID =
    decode WebSessionID
        |> required "session_id" D.string


decoderData : Decoder (String -> WebData)
decoderData =
    D.oneOf
        [ decoderWebsocketEvent
        , decoderWebsocketPricesMarket
        , decoderWebsocketSessionID
        ]



--decoderWebData : Decoder WebData


decodeWebData : String -> Result String WebData
decodeWebData =
    decode (\f hashCode -> f hashCode)
        |> required "ok" decoderData
        |> required "hash_code" D.string
        |> D.decodeString
