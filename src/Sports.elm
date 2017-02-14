module Sports exposing (Model, Msg, view, init, update)

import Aping
import Html
    exposing
        ( Html
        , Attribute
        , a
        , li
        , ul
        , span
        , text
        )
import Html.Attributes exposing (class, classList, href, style, attribute, colspan, href, id)
import Aping exposing (Event)
import Aping.Decoder
import Navigation exposing (Location)
import Json.Decode
import Http
import Debug


type alias Model =
    { sports : List Aping.Sport
    }


type Msg
    = NewSports (Result String (List Aping.Sport))


init : Location -> ( Model, Cmd Msg )
init location =
    { sports = [] } ! [ httpRequestSports location ]


httpRequestSports : Location -> Cmd Msg
httpRequestSports location =
    let
        url =
            location.protocol ++ "//" ++ location.host ++ "/sports"

        decoder =
            Json.Decode.list Aping.Decoder.sport
                |> Json.Decode.field "ok"
    in
        Http.get url decoder
            |> Http.send (Result.mapError toString >> NewSports)


linkSport : Int -> Aping.Sport -> Html a
linkSport sportID { name, id } =
    let
        href_ =
            href <| "#sport/" ++ toString id

        text_ =
            text name

        isActive =
            sportID == id
    in
        li
            [ classList [ ( "active", isActive ) ] ]
            [ a [ href_ ]
                [ if isActive then
                    a [ href_ ] [ text_ ]
                  else
                    text_
                ]
            ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg m =
    case msg of
        NewSports (Ok sports) ->
            { m | sports = sports } ! []

        NewSports (Err error) ->
            let
                _ =
                    Debug.log "SPORTS error" error
            in
                m ! []



-- VIEW


view : Int -> Model -> Html a
view sportID { sports } =
    let
        sports_ =
            List.sortBy (\{ market_count } -> market_count * (-1)) sports

        xs =
            List.drop 6 sports_

        xs1 =
            sports_
                |> List.take 6
                |> List.map (linkSport sportID)

        vx =
            case List.partition (\{ id } -> id == sportID) xs of
                ( [], xs2 ) ->
                    xs1 ++ [ view2 xs2 ]

                ( sport :: _, xs2 ) ->
                    xs1
                        ++ [ linkSport sportID sport
                           , view2 xs2
                           ]
    in
        ul [ class "nav nav-tabs" ] vx


view2 : List Aping.Sport -> Html a
view2 xs =
    li
        [ class "dropdown" ]
        [ a [ class "dropdown-toggle", attribute "data-toggle" "dropdown", href "#" ]
            [ text "Другие рынки...    "
            , span [ class "caret" ]
                []
            ]
        , List.map (linkSport 0) xs
            |> ul [ class "dropdown-menu" ]
        ]
