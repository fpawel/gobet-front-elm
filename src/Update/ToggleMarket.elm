module Update.ToggleMarket exposing (update)

import Http
import Json.Decode
import Data.Aping
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

                nextMarketsPrices =
                    marketPrices
                        |> Maybe.map (\_ -> Dict.remove marketID p.marketsPrices)
                        |> Maybe.withDefault p.marketsPrices

                nextMarketExapnded =
                    isJust marketPrices |> not

                cmds =
                    if String.isEmpty p.session then
                        []
                    else
                        [ webPostToggleMarket m marketID ]
            in
                { m | page = PageEvent { p | marketsPrices = nextMarketsPrices } }
                    ! [ webPostToggleMarket m marketID ]

        _ ->
            Debug.crash "updateToggleMarket" m.page


webPostToggleMarket : Model -> String -> Cmd Msg
webPostToggleMarket m marketID =
    case m.page of
        PageEvent p ->
            let
                url =
                    m.location.protocol ++ "//" ++ m.location.host ++ "/wsprices-markets/"

                bodyData =
                    { id = marketID
                    , include = False
                    , session = p.session
                    }

                body =
                    Http.stringBody
                        "application/json"
                        (Data.Prices.encodeToggleMarket bodyData)

                decoder =
                    Data.Aping.decoderEvent
                        |> Json.Decode.field "ok"
            in
                Http.post
                    url
                    body
                    decoder
                    |> Http.send
                        (Result.mapError toString
                            >> fromResult
                                WebDataError
                                (\_ -> ToggleMarketPosted marketID)
                        )

        _ ->
            Debug.crash "webPostToggleMarket" m.page
