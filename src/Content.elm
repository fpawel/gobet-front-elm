module Content exposing (..)

import Html exposing (Html)
import Debug
import Navigation exposing (Location)
import Football as MFootball
import Sport as MSport
import Msg exposing (Msg)
import Routing exposing (Route)
import Aping
import View.Sports
import Time


type Model
    = Sport MSport.Model
    | Football MFootball.Model


sport :
    { location : Location
    , sport : Aping.Sport
    , time : Time.Time
    }
    -> ( Model, Cmd Msg )
sport x =
    let
        ( model_sport, cmd_sport ) =
            MSport.init x
    in
        Sport model_sport ! [ Cmd.map Msg.Sport cmd_sport ]


football : Location -> Model
football location =
    Football (MFootball.init location)


route : Model -> Route
route model =
    case model of
        Sport { sport } ->
            Routing.Sport sport.id

        Football _ ->
            Routing.Football


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Msg.Sport msgSport, Sport m ) ->
            let
                ( um, cmd ) =
                    MSport.update msgSport m
            in
                Sport um ! [ Cmd.map Msg.Sport cmd ]

        ( Msg.Football msgFootball, Football m ) ->
            let
                ( um, cmd ) =
                    MFootball.update msgFootball m
            in
                Football um ! [ Cmd.map Msg.Football cmd ]

        x ->
            Debug.crash <| "wrong content message: " ++ toString x


view :
    List Aping.Sport
    -> Model
    -> List (Html Msg)
view sports model =
    case model of
        Sport ({ sport } as m) ->
            [ View.Sports.view { sports = sports, sportID = sport.id }
            , MSport.view m
                |> Html.map Msg.Sport
            ]

        Football m ->
            [ MFootball.view m ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Sport m ->
            Sub.map Msg.Sport (MSport.subscriptions m)

        Football m ->
            Sub.map Msg.Football (MFootball.subscriptions m)
