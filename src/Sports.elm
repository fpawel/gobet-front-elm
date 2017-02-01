module Sports exposing (..)

import Aping
import Http
import Regex exposing (..)
import Json.Decode
import Html.Events exposing (onClick)
import Html
    exposing
        ( Html
        , Attribute
        , a
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
import Date
import Dict exposing (Dict)


-- import Set exposing (Set)

import Time exposing (Time)
import Navigation exposing (Location)
import Aping exposing (Event)
import Aping.Decoder
import Aping.Events
import Html.Attributes exposing (class, href, style, attribute, colspan, href, id)


type alias Config =
    { sports : List Aping.Sport
    , sportID : Int
    }


view1 : Config -> Html a
view1 { sports, sportID } =
    sports
        |> List.take 5
        |> List.map
            (\{ id, name } ->
                a [ href <| "#sport/" ++ toString id ]
                    [ text name ]
                    |> List.singleton
                    |> li
                        (if sportID == id then
                            [ class "active" ]
                         else
                            []
                        )
            )
        |> ul [ class "nav nav-tabs" ]


view2 : Config -> Html a
view2 { sports, sportID } =
    sports
        |> List.drop 5
        |> List.filter (\{ id } -> not (id == sportID))
        |> List.map
            (\{ id, name } ->
                a [ href <| "#sport/" ++ toString id ]
                    [ text name ]
                    |> List.singleton
                    |> li
                        (if sportID == id then
                            [ class "active" ]
                         else
                            []
                        )
            )
        |> ul [ class "nav nav-tabs" ]
