module Msg exposing (Msg(..))

import Navigation
import Football as MFootball
import Sport as MSport
import Sports as MSports
import Event as MEvent


type Msg
    = Sports MSports.Msg
    | Sport MSport.Msg
    | Event MEvent.Msg
    | Football MFootball.Msg
    | OnLocationChanged Navigation.Location
