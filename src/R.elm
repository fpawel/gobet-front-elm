module R exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (Parser, map, (<?>), (</>), s, int, string, parseHash, oneOf, stringParam)


parse1 : Parser (a -> a) a -> String -> Maybe a
parse1 parser x =
    parseHash parser { loc | hash = x }


loc : Navigation.Location
loc =
    { href = ""
    , host = ""
    , hostname = ""
    , protocol = ""
    , origin = ""
    , port_ = ""
    , pathname = ""
    , search = ""
    , hash = ""
    , username = ""
    , password = ""
    }
