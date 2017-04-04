module Update.ToggleMarket exposing (update)

import Http
import Json.Decode
import Data.Prices
import Help.Utils exposing (websocketURL, isJust, fromResult)
import App exposing (Msg(..), Page(..), Model)
import Dict
import String


update : Model -> String -> ( Model, Cmd Msg )
update m marketID =
    case m.page of
        PageEvent p ->
            let
                marketPrices =
                    Dict.get marketID p.marketsPrices

                marketsPrices =
                    Data.Prices.toggleMarket marketID p.marketsPrices

                include =
                    Dict.get marketID marketsPrices
                        |> isJust

                cmds =
                    if String.isEmpty p.session then
                        []
                    else
                        [ webPostToggleMarket m marketID include ]
            in
                { m | page = PageEvent { p | marketsPrices = marketsPrices } }
                    ! cmds

        _ ->
            Debug.crash "updateToggleMarket" m.page


webPostToggleMarket : Model -> String -> Bool -> Cmd Msg
webPostToggleMarket m marketID include =
    case m.page of
        PageEvent p ->
            let
                url =
                    m.location.protocol ++ "//" ++ m.location.host ++ "/prices-markets"

                bodyData =
                    { id = marketID
                    , include = include
                    , session = p.session
                    }

                body =
                    Http.stringBody
                        "application/json"
                        (Data.Prices.encodeToggleMarket bodyData)

                decoder =
                    Json.Decode.succeed (ToggleMarketPosted marketID)
                        |> Json.Decode.field "ok"
            in
                Http.post
                    url
                    body
                    decoder
                    |> Http.send
                        (Result.mapError toString
                            >> fromResult WebDataError identity
                        )

        _ ->
            Debug.crash "webPostToggleMarket" m.page
