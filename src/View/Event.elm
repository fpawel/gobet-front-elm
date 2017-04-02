module View.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation exposing (Location)
import Data.Aping
import App
import DateUtils


-- VIEW


view1 : Data.Aping.Event -> List Data.Aping.Market -> Html App.Msg
view1 event markets =
    let
        date =
            DateUtils.formatDayMonthYear <| DateUtils.dateFromDate event.openDate

        header =
            div [ class "page-header" ]
                [ h1 [] [ text <| event.name ]
                ]

        date_country =
            div [ style [ ( "text-align", "right" ) ] ]
                [ text (event.country ++ ", " ++ date) ]

        ( ms1_, ms2_ ) =
            markets
                |> List.indexedMap
                    (\n mmarket ->
                        ( n % 2
                        , Market.view mmarket
                            |> Html.map (MsgMarket mmarket.market.id)
                        )
                    )
                |> List.partition (Tuple.first >> (==) 0)

        ms1 =
            List.map Tuple.second ms1_

        ms2 =
            List.map Tuple.second ms2_
    in
        div []
            [ header
            , date_country
            , div
                [ class "row"
                , style [ ( "margin-top", "5px" ) ]
                ]
                [ div [ class "col-sm-6" ] ms1
                , div [ class "col-sm-6" ] ms2
                ]
            ]


view : Model -> Html Msg
view { event, markets } =
    case event of
        Just event ->
            view1 event markets

        _ ->
            spinner_text "Подготовка данных..."
