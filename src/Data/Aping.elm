module Data.Aping exposing (..)

import Date
import Regex exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import Help.CountryCode
import Dict exposing (Dict)


type alias Sport =
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
    , sport : Sport
    , markets : List Market
    }


type alias Market =
    { id : String
    , name : String
    , totalMatched : Float
    , totalAvailable : Float
    , runners : List Runner
    , competition : String
    }


type alias Runner =
    { id : Int
    , name : String
    }


getSportByID : Int -> List Sport -> Sport
getSportByID sportID sports =
    case List.filter (\{ id } -> id == sportID) sports of
        decoderSport :: _ ->
            decoderSport

        _ ->
            Debug.crash <| "unknown decoderSport id " ++ toString sportID


eventTeams : String -> Maybe ( String, String )
eventTeams s =
    case split (AtMost 1) (regex " [v@\\-] ") s of
        [ s1, s2 ] ->
            Just ( s1, s2 )

        _ ->
            Nothing


insertEvents : Dict Int Event -> Dict Int Event -> Dict Int Event
insertEvents events newEvents =
    Dict.merge
        Dict.insert
        (\k _ x -> Dict.insert k x)
        Dict.insert
        events
        newEvents
        Dict.empty


chooseMarkets : List Market -> List Market
chooseMarkets =
    List.filter (\{ name } -> name /= "Азиатский гандикап")
        >> List.sortBy
            (\{ name, totalMatched } ->
                ( case name of
                    "Ставки" ->
                        0

                    "Результат" ->
                        1

                    _ ->
                        2
                , (-1) * totalMatched
                , name
                )
            )



-- DECODERS


decoderCountryName : Decoder String
decoderCountryName =
    D.map
        (\x ->
            Help.CountryCode.countryName x
                |> Maybe.withDefault x
        )
        D.string


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
        |> optional "event_type" decoderSport (Sport 0 "" 0)
        |> optional "markets" (D.list decoderMarket) []


decoderSport : Decoder Sport
decoderSport =
    decode Sport
        |> required "id" D.int
        |> optional "name" D.string ""
        |> optional "market_count" D.int 0


decoderRunner : Decoder Runner
decoderRunner =
    decode Runner
        |> required "selectionId" D.int
        |> optional "runnerName" D.string ""


decoderMarket : Decoder Market
decoderMarket =
    decode Market
        |> required "marketId" D.string
        |> optional "marketName" D.string ""
        |> optional "totalMatched" D.float 0
        |> optional "totalAvailable" D.float 0
        |> optional "runners" (D.list decoderRunner) []
        |> optional "competition" D.string ""
