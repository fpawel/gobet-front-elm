module Market exposing (..)

import Html exposing (Html, Attribute, text, span, div, table, td, tr, th, h3, tbody, a)
import Html.Attributes as Attr exposing (class, colspan, href, style, attribute)
import Html.Events exposing (onClick)


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
    = ToggleCollapse String Bool


init : Location -> Aping.Market -> ( Model, Cmd Msg )
init location market =
    Model market location False ! []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        ToggleCollapse _ _ ->
            { m
                | isExpanded = not m.isExpanded
            }
                ! []


view : Model -> Html Msg
view { market, isExpanded } =
    let
        runners =
            if isExpanded then
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
                    |> table []
                    |> List.singleton
                    |> div [ class "panel-body" ]
                    |> List.singleton
            else
                []

        marketName =
            td
                [ attribute "width" "100%" ]
                [ span [ class "caret" ] []
                , span [ style [ ( "margin-left", "5px" ) ] ] [ text market.name ]
                ]

        totalMatched =
            round <| market.totalMatched

        head1 =
            if totalMatched /= 0 then
                [ marketName
                , td
                    [ style [ ( "color", "yellow" ) ]
                    ]
                    [ text <| (toString totalMatched) ++ "$"
                    ]
                ]
            else
                [ marketName ]

        heading =
            div
                [ class <|
                    "panel-heading "
                        ++ (if isExpanded then
                                "dropup"
                            else
                                "dropdown"
                           )
                , onClick (ToggleCollapse market.id (not isExpanded))
                , style [ ( "cursor", "pointer" ) ]
                , attribute "width" "100%"
                ]
                [ table
                    [ attribute "width" "100%" ]
                    [ tbody [] [ tr [] head1 ] ]
                ]
    in
        div
            [ class "panel panel-primary" ]
            ([ heading ] ++ runners)
