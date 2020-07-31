module Main exposing (main)

import Browser
import Html.Styled exposing (toUnstyled)
import Page.Search as Search


main : Program () Search.Model Search.Msg
main =
    Browser.element
        { init = Search.initModel
        , view = Search.view >> toUnstyled
        , update = Search.update
        , subscriptions = \_ -> Sub.none
        }
