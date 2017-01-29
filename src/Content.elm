module Content exposing (..)

import Html exposing (Html)
import Debug
import Navigation
import Football as MFootball
import Sport as MSport
import Msg exposing (Msg)
import Routing exposing (Route)
import ApiNgTypes exposing (EventType)


type Model
    = Football MFootball.Model
    | Sport MSport.Model


init : Navigation.Location -> List EventType -> Route -> Model
init location eventTypes route =
    case route of
        Routing.Football ->
            Football <| MFootball.init location

        Routing.Sport eventType_id ->
            case List.filter (\{ id } -> id == eventType_id) eventTypes of
                eventType :: _ ->
                    Sport <| MSport.init eventType

                _ ->
                    Debug.crash <| "unknown event type id " ++ toString eventType_id


route : Model -> Route
route model =
    case model of
        Football _ ->
            Routing.Football

        Sport m ->
            let
                { id } =
                    MSport.eventType m
            in
                Routing.Sport id


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( Msg.Football msg_football, Football model_football ) ->
            let
                ( updated_football, cmd_football ) =
                    MFootball.update msg_football model_football
            in
                Football updated_football ! [ Cmd.map Msg.Football cmd_football ]

        ( Msg.Sport msg_sport, Sport model_sport ) ->
            let
                ( updated_sport, cmd_sport ) =
                    MSport.update msg_sport model_sport
            in
                Sport updated_sport ! [ Cmd.map Msg.Sport cmd_sport ]

        x ->
            Debug.crash <| "wrong content message: " ++ toString x


view : Model -> Html Msg
view model =
    case model of
        Football model_football ->
            MFootball.view model_football

        Sport model_sport ->
            MSport.view model_sport



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Football m ->
            Sub.map Msg.Football <| MFootball.subscriptions m

        _ ->
            Sub.none
