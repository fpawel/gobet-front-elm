module Market exposing (..)

import Html exposing (Html, Attribute, text, span, div, table, td, tr, th, h3, tbody)
import Html.Attributes as Attr exposing (class, colspan)


--import Http
--import Json.Decode

import Navigation exposing (Location)
import Aping


--import Aping.Decoder
-- MODEL


type alias Model =
    { market : Aping.Market
    , location : Location
    , isExpanded : Bool
    }


type Msg
    = ToggleCollapse Int


init : Location -> Aping.Market -> ( Model, Cmd Msg )
init location market =
    Model market location False ! []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        ToggleCollapse id ->
            { m
                | isExpanded =
                    if id == m.market.id then
                        not m.isExpanded
                    else
                        m.isExpanded
            }
                ! []


view : Model -> Html Msg
view { market } =
    let
        runners =
            market.runners
                |> List.map
                    (\{ name, id } ->
                        tr
                            []
                            [ td [] [ text name ]
                            ]
                    )
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
