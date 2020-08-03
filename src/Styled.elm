module Styled exposing (StyledEl)

import Html.Styled exposing (Attribute, Html)


type alias StyledEl msg =
    List (Attribute msg) -> List (Html msg) -> Html msg
