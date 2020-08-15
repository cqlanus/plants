module Pagination exposing (pageText, pagingButton, pagingContainer)

import Css exposing (..)
import Html.Styled exposing (button, div, styled)
import Styled exposing (StyledEl)


pagingContainer : StyledEl div
pagingContainer =
    styled div
        [ displayFlex
        , alignItems center
        ]


pageText : StyledEl div
pageText =
    styled div
        [ flex (num 1) ]


pagingButton : StyledEl button
pagingButton =
    styled button
        [ flex (num 1)
        , padding (rem 1)
        , backgroundColor (hex "#fff")
        , border (px 0)
        , fontSize (rem 1)
        , cursor pointer
        , hover
            [ backgroundColor (hex "#eee") ]
        ]
