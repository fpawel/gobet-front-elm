module Styles exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


animated : String
animated =
    "animated"


jello : String
jello =
    "jello"


flash : String
flash =
    "flash"


pulse : String
pulse =
    "pulse"


bounceInUp : String
bounceInUp =
    "bounceInUp"


animated_jello_2s : List (Html.Attribute msg)
animated_jello_2s =
    [ class (animated ++ " " ++ jello)
    , style [ ( "-webkit-animation-duration", "2s" ) ]
    ]


animated_flash_2s : List (Html.Attribute msg)
animated_flash_2s =
    [ class (animated ++ " " ++ flash)
    , style [ ( "-webkit-animation-duration", "2s" ) ]
    ]


animated_bounceInUp_2s : List (Html.Attribute msg)
animated_bounceInUp_2s =
    [ class (animated ++ " " ++ bounceInUp)
    , style [ ( "-webkit-animation-duration", "2s" ) ]
    ]
