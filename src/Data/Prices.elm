module Data.Prices
    exposing
        ( WebData(..)
        , Market
        , AppMarket
        , MarketPrices
        , PriceSize
        , decodeWebData
        , encodeToggleMarket
        , updateAppMarkets
        )

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (object, encode)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import Data.Aping exposing (Event, decoderEvent)
import Dict exposing (Dict)


type WebData
    = WebEvent Data.Aping.Event String
    | WebMarket Market String
    | WebSessionID String String


type alias AppMarkets =
    Dict String AppMarket


type alias Markets =
    Dict String Market


type alias Market =
    { id : String
    , totalMatched : Maybe Float
    , totalAvailable : Maybe Float
    , runners : List Runner
    }


type alias AppMarket =
    { id : String
    , totalMatched : Maybe Float
    , totalAvailable : Maybe Float
    , prices : MarketPrices
    }


type alias MarketPrices =
    Dict PriceIndex PriceSize


type alias PriceIndex =
    ( Int, String, Int )


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


updateAppMarkets : AppMarkets -> Market -> AppMarkets
updateAppMarkets markets market =
    let
        m =
            Dict.get market.id markets
                |> Maybe.withDefault
                    { id = market.id
                    , totalMatched = Nothing
                    , totalAvailable = Nothing
                    , prices = Dict.empty
                    }
                |> updateAppMarket market
    in
        Dict.insert market.id m markets


updateAppMarket : Market -> AppMarket -> AppMarket
updateAppMarket { id, totalMatched, totalAvailable, runners } x =
    { x
        | id = id
        , totalMatched = totalMatched
        , totalAvailable = totalAvailable
        , prices =
            runners
                |> indexedPrices
                |> Dict.fromList
                |> updateMarketPrices x.prices
    }


indexedPrices : List Runner -> List ( PriceIndex, Maybe PriceSize )
indexedPrices =
    List.concatMap
        (\runner ->
            runner.odds
                |> List.map
                    (\odd ->
                        ( ( runner.id, odd.side, odd.index ), odd.odd )
                    )
        )


updateMarketPrices : MarketPrices -> Dict PriceIndex (Maybe PriceSize) -> MarketPrices
updateMarketPrices ms rs =
    let
        maybeInsert k =
            Maybe.map
                (Dict.insert k)
                >> Maybe.withDefault identity
    in
        Dict.merge
            Dict.insert
            (\k _ -> maybeInsert k)
            maybeInsert
            ms
            rs
            Dict.empty


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
        |> optional "total_matched" (D.map Just D.float) Nothing
        |> optional "total_available" (D.map Just D.float) Nothing
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
