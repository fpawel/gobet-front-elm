module View.Sports exposing (view)

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


type alias Config =
    { sports : List Aping.Sport
    , sportID : Int
    }


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


view : Config -> Html a
view ({ sports, sportID } as config) =
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
