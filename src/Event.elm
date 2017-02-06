module Event exposing (..)

import Http
import Json.Decode
import Navigation exposing (Location)
import Aping
import Aping.Decoder
import Market


-- MODEL


type alias Model =
    { event : Aping.Event
    , markets : List Market.Model
    , error : Maybe String
    , location : Location
    }


type Msg
    = NewMarkets (Result String (List Aping.Market))
    | MsgMarket Market.Msg


init : Location -> Aping.Event -> ( Model, Cmd Msg )
init location event =
    Model event [] Nothing location ! [ httpRequestMarkets location event.id ]


httpRequestMarkets : Location -> Int -> Cmd Msg
httpRequestMarkets location eventID =
    let
        marketsURL =
            location.protocol ++ "//" ++ location.host ++ "/markets/" ++ toString eventID

        decoder =
            Json.Decode.list Aping.Decoder.market
                |> Json.Decode.field "result"
    in
        Http.get marketsURL decoder
            |> Http.send (Result.mapError toString >> NewMarkets)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        MsgMarket msgMarket ->
            let
                ( markets, cmds ) =
                    List.map Market.update m.markets
                        |> List.unzip
            in
                { m | markets = markets } ! []

        NewMarkets (Ok markets) ->
            { m
                | markets = List.map (Market.init m.location >> Tuple.first) markets
                , error = Nothing
            }
                ! []

        NewMarkets (Err error) ->
            { m | error = Just <| Debug.log "MARKETS error" <| toString error }
                ! [ httpRequestMarkets m.location m.event.id ]


view : Model -> Html Msg
view { event, markets } =
    let
        markets_ =
            markets
                |> List.map
                    (Market.view >> Html.map MsgMarket)
                |> tbody []
                |> List.singleton
                |> table [ class "panel-body" ]
    in
        div
            [ class "panel panel-primary" ]
            [ div
                [ class "panel-heading" ]
                [ text market.name ]
            , div
                [ class "panel-body" ]
                [ runners ]
            ]
