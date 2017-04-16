module Data.Football exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import Data.Aping exposing (Event, decoderEvent)


type alias Game =
    { eventID : Int
    , marketID : Int
    , home : String
    , away : String
    , page : Int
    , order : Int
    , result : String
    , time : String
    , win1 : Maybe Float
    , win2 : Maybe Float
    , draw1 : Maybe Float
    , draw2 : Maybe Float
    , lose1 : Maybe Float
    , lose2 : Maybe Float
    , inplay : Bool
    , utime : Bool
    , uresult : Bool
    , uwin1 : Bool
    , uwin2 : Bool
    , udraw1 : Bool
    , udraw2 : Bool
    , ulose1 : Bool
    , ulose2 : Bool
    }


type alias MaybeMaybeFloat =
    Maybe (Maybe Float)


type alias GameUpdates =
    { eventID : Int
    , page : Maybe Int
    , order : Maybe Int
    , time : Maybe String
    , result : Maybe String
    , win1 : MaybeMaybeFloat
    , win2 : MaybeMaybeFloat
    , draw1 : MaybeMaybeFloat
    , draw2 : MaybeMaybeFloat
    , lose1 : MaybeMaybeFloat
    , lose2 : MaybeMaybeFloat
    }


type alias GameListUpdates =
    { inplay : List Game
    , outplay : List Int
    , changes : List GameUpdates
    , events : List Event
    }


type alias WebData =
    { changes : GameListUpdates
    , hashCode : String
    }


updateGame : Game -> GameUpdates -> Game
updateGame x y =
    let
        comp fy fx =
            case fy y of
                Just t ->
                    t /= fx x

                _ ->
                    False
    in
        { x
            | page = Maybe.withDefault x.page y.page
            , order = Maybe.withDefault x.order y.order
            , time = Maybe.withDefault x.time y.time
            , result = Maybe.withDefault x.result y.result
            , win1 = Maybe.withDefault x.win1 y.win1
            , win2 = Maybe.withDefault x.win2 y.win2
            , draw1 = Maybe.withDefault x.draw1 y.draw1
            , draw2 = Maybe.withDefault x.draw2 y.draw2
            , lose1 = Maybe.withDefault x.lose1 y.lose1
            , lose2 = Maybe.withDefault x.lose2 y.lose2
            , utime = comp .time .time
            , uresult = comp .result .result
            , uwin1 = comp .win1 .win1
            , uwin2 = comp .win2 .win2
            , udraw1 = comp .draw1 .draw1
            , udraw2 = comp .draw2 .draw2
            , ulose1 = comp .lose1 .lose1
            , ulose2 = comp .lose2 .lose2
        }


updateGamesList : GameListUpdates -> List Game -> List Game
updateGamesList { inplay, outplay, changes } games =
    let
        outplaySet =
            outplay
                |> Set.fromList

        inplaySet =
            Set.fromList (List.map (\x -> x.eventID) inplay)

        changesMap =
            changes
                |> List.map (\x -> ( x.eventID, x ))
                |> Dict.fromList

        isJust =
            Maybe.map (\_ -> True)
                >> Maybe.withDefault False

        play =
            games
                |> List.filter
                    (\{ eventID } ->
                        (not <| Set.member eventID outplaySet)
                            && (not <| Set.member eventID inplaySet)
                    )
                |> List.map
                    (\x ->
                        Dict.get x.eventID changesMap
                            |> Maybe.map (updateGame x)
                            |> Maybe.withDefault x
                    )
    in
        inplay ++ play


parseWebData : String -> Result String WebData
parseWebData =
    D.decodeString decoderWebData



-- DECODERS


decoderGame : Decoder Game
decoderGame =
    decode Game
        |> required "event_id" D.int
        |> required "market_id" D.int
        |> required "home" D.string
        |> required "away" D.string
        |> required "page" D.int
        |> required "order" D.int
        |> optional "result" D.string ""
        |> required "time" D.string
        |> optional "win1" (D.maybe D.float) Nothing
        |> optional "win2" (D.maybe D.float) Nothing
        |> optional "draw1" (D.maybe D.float) Nothing
        |> optional "draw2" (D.maybe D.float) Nothing
        |> optional "lose1" (D.maybe D.float) Nothing
        |> optional "lose2" (D.maybe D.float) Nothing
        |> hardcoded True
        |> hardcoded False
        |> hardcoded False
        |> hardcoded False
        |> hardcoded False
        |> hardcoded False
        |> hardcoded False
        |> hardcoded False
        |> hardcoded False


decoderMaybeFloat : Decoder (Maybe (Maybe Float))
decoderMaybeFloat =
    (D.maybe (D.maybe D.float))


decoderGameCahnges : Decoder GameUpdates
decoderGameCahnges =
    decode GameUpdates
        |> required "event_id" D.int
        |> optional "page" (D.maybe D.int) Nothing
        |> optional "order" (D.maybe D.int) Nothing
        |> optional "time" (D.maybe D.string) Nothing
        |> optional "result" (D.maybe D.string) Nothing
        |> optional "win1" decoderMaybeFloat Nothing
        |> optional "win2" decoderMaybeFloat Nothing
        |> optional "draw1" decoderMaybeFloat Nothing
        |> optional "draw2" decoderMaybeFloat Nothing
        |> optional "lose1" decoderMaybeFloat Nothing
        |> optional "lose2" decoderMaybeFloat Nothing


decoderGameListUpdates : Decoder GameListUpdates
decoderGameListUpdates =
    decode
        GameListUpdates
        |> optional "inplay" (D.list decoderGame) []
        |> optional "outplay" (D.list D.int) []
        |> optional "game_changes" (D.list decoderGameCahnges) []
        |> optional "events" (D.list decoderEvent) []


decoderWebData : Decoder WebData
decoderWebData =
    decode
        WebData
        |> required "changes" decoderGameListUpdates
        |> required "hash_code" D.string
