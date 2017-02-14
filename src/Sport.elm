module Sport
    exposing
        ( Model
        , Msg
        , update
        , init
        , view
        , subscriptions
        )

import Http
import Json.Decode
import Html
    exposing
        ( Html
        , Attribute
        , button
        , ul
        , li
        , h1
        , h3
        , span
        , div
        , nav
        , tbody
        , tr
        , td
        , th
        , thead
        , table
        , text
        )
import Navigation exposing (Location)
import Aping exposing (Event)
import Aping.Decoder
import Help.Component exposing (mainMenuItem, spinner_text)
import Table exposing (defaultCustomizations)
import View.SportTable


-- MODEL


type alias Model =
    { location : Location
    , sport : Aping.Sport
    , events : List Event
    , tableState : Table.State
    }


type Msg
    = NewEvents (Result String (List Event))
    | SetTableState Table.State


init :
    { location : Location
    , sport : Aping.Sport
    }
    -> ( Model, Cmd Msg )
init { location, sport } =
    { location = location
    , sport = sport
    , events = []
    , tableState = Table.initialSort "Дата открытия"
    }
        ! [ httpRequestEvents location sport ]


httpRequestEvents :
    Location
    -> Aping.Sport
    -> Cmd Msg
httpRequestEvents location eventType =
    let
        eventsURL =
            location.protocol ++ "//" ++ location.host ++ "/events/" ++ toString eventType.id

        decoder =
            Json.Decode.list Aping.Decoder.event
                |> Json.Decode.field "ok"
    in
        Http.get eventsURL decoder
            |> Http.send (Result.mapError toString >> NewEvents)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NewEvents (Ok events) ->
            { m | events = events } ! []

        NewEvents (Err error) ->
            let
                _ =
                    Debug.log ("SPORT " ++ toString m.sport ++ " error") error
            in
                m ! []

        SetTableState newState ->
            { m | tableState = newState } ! []



-- VIEW


view : Model -> Html Msg
view ({ events, tableState } as model) =
    if List.isEmpty events then
        spinner_text "Подготовка данных..."
    else
        Table.view
            (View.SportTable.config SetTableState)
            tableState
            events



-- SUBSCRIPTINS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
