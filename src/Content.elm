module Content exposing (..)

import Html exposing (Html)
import Debug
import Navigation exposing (Location)


--import Football as MFootball

import Sport as MSport
import Msg exposing (Msg)
import Routing exposing (Route)
import Aping
import View.Sports
import Time


type Model
    = Sport MSport.Model


initSport :
    { location : Location
    , sport : Aping.Sport
    , time : Time.Time
    }
    -> ( Model, Cmd Msg )
initSport x =
    let
        ( model_sport, cmd_sport ) =
            MSport.init x
    in
        Sport model_sport ! [ Cmd.map Msg.Sport cmd_sport ]


init :
    { location : Location
    , sports : List Aping.Sport
    , time : Time.Time
    }
    -> Route
    -> ( Model, Cmd Msg )
init { location, sports, time } route =
    case route of
        Routing.Sport sportID ->
            let
                sport =
                    Aping.getSportByID sportID sports

                ( model_sport, cmd_sport ) =
                    MSport.init
                        { location = location
                        , sport = sport
                        , time = time
                        }
            in
                Sport model_sport ! [ Cmd.map Msg.Sport cmd_sport ]


route : Model -> Route
route model =
    case model of
        Sport { sport } ->
            Routing.Sport sport.id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Msg.Sport msg_sport, Sport model_sport ) ->
            let
                ( updated_sport, cmd_sport ) =
                    MSport.update msg_sport model_sport
            in
                Sport updated_sport ! [ Cmd.map Msg.Sport cmd_sport ]

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Sport model_sport ->
            Sub.map Msg.Sport (MSport.subscriptions model_sport)
