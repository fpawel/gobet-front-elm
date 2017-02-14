module Content exposing (..)

import Html exposing (Html, button)
import Debug
import Navigation exposing (Location)
import Football as MFootball
import Sport as MSport
import Event as MEvent
import Msg exposing (Msg)
import Routing exposing (Route)
import Aping


type Model
    = Sport MSport.Model
    | Event MEvent.Model
    | Football MFootball.Model


sportID : Model -> Int
sportID m =
    case m of
        Sport m ->
            m.sport.id

        Event m ->
            m.event |> Maybe.map (\x -> x.sport.id) |> Maybe.withDefault 0

        Football _ ->
            1


sport :
    { location : Location
    , sport : Aping.Sport
    }
    -> ( Model, Cmd Msg )
sport x =
    let
        ( model_sport, cmd_sport ) =
            MSport.init x
    in
        Sport model_sport ! [ Cmd.map Msg.Sport cmd_sport ]


event : Location -> Int -> ( Model, Cmd Msg )
event location eventID =
    let
        ( model_event, cmd_event ) =
            MEvent.init location eventID
    in
        Event model_event ! [ Cmd.map Msg.Event cmd_event ]


football : Location -> Model
football location =
    Football (MFootball.init location)


route : Model -> Route
route model =
    case model of
        Sport { sport } ->
            Routing.Sport sport.id

        Event { eventID } ->
            Routing.Event eventID

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

        ( Msg.Event msgEvent, Event m ) ->
            let
                ( um, cmd ) =
                    MEvent.update msgEvent m
            in
                Event um ! [ Cmd.map Msg.Event cmd ]

        ( Msg.Football msgFootball, Football m ) ->
            let
                ( um, cmd ) =
                    MFootball.update msgFootball m
            in
                Football um ! [ Cmd.map Msg.Football cmd ]

        x ->
            Debug.crash <| "wrong content message: " ++ toString x


view : List Aping.Sport -> Model -> Html Msg
view sports model =
    case model of
        Sport m ->
            MSport.view m
                |> Html.map Msg.Sport

        Event m ->
            Html.map Msg.Event <| MEvent.view m

        Football m ->
            MFootball.view m



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Sport m ->
            Sub.map Msg.Sport (MSport.subscriptions m)

        Event m ->
            Sub.map Msg.Event (MEvent.subscriptions m)

        Football m ->
            Sub.map Msg.Football (MFootball.subscriptions m)
