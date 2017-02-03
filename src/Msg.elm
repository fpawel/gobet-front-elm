module Msg exposing (Msg(..))

import Navigation
import Football as MFootball
import Sport as MSport
import Time


type Msg
    = Sport MSport.Msg
    | Football MFootball.Msg
    | UrlChange Navigation.Location
    | Tick Time.Time
