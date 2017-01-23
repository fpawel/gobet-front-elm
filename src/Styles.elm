module Styles exposing (..)

import Html exposing (Attribute)
import Html.Attributes as Attr


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
    [ Attr.class (animated ++ " " ++ jello)
    , Attr.style [ ( "-webkit-animation-duration", "2s" ) ]
    ]


animated_flash_2s : List (Html.Attribute msg)
animated_flash_2s =
    [ Attr.class (animated ++ " " ++ flash)
    , Attr.style [ ( "-webkit-animation-duration", "2s" ) ]
    ]


animated_bounceInUp_2s : List (Html.Attribute msg)
animated_bounceInUp_2s =
    [ Attr.class (animated ++ " " ++ bounceInUp)
    , Attr.style [ ( "-webkit-animation-duration", "2s" ) ]
    ]
