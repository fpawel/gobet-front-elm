module Msg exposing (Msg(..))

import Navigation
import Football as F
import Sport as S


type Msg
    = Football F.Msg
    | Sport S.Msg
    | UrlChange Navigation.Location
