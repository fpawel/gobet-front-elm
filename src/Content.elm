module Content exposing (..)

import Html exposing (Html)
import Debug
import Navigation


--import Football as MFootball

import Sport as MSport
import Msg exposing (Msg)
import Routing exposing (Route)
import Aping


type Model
    = Sport MSport.Model


init :
    { a
        | location : Navigation.Location
        , sports : List Aping.Sport
    }
    -> Route
    -> ( Model, Cmd Msg )
init { location, sports } route =
    case route of
        Routing.Sport sportID ->
            case List.filter (\{ id } -> id == sportID) sports of
                sport :: _ ->
                    let
                        ( model_sport, cmd_sport ) =
                            MSport.init
                                { location = location
                                , sport = sport
                                , sports = sports
                                }
                    in
                        Sport model_sport ! [ Cmd.map Msg.Sport cmd_sport ]

                _ ->
                    Debug.crash <| "unknown sport id " ++ toString sportID


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


view : Model -> Html Msg
view model =
    case model of
        Sport model_sport ->
            MSport.view model_sport
                |> Html.map Msg.Sport



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Sport m ->
            Sub.map Msg.Sport <| MSport.subscriptions m


what : Model -> String
what x =
    case x of
        Sport { sport } ->
            sport.name
