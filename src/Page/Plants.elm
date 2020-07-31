module Page.Plants exposing (Model, Msg, initModel, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, div, text)
import Plant exposing (Plant)


type alias Model =
    { plants : List Plant
    , navKey : Nav.Key
    }


initModel : Nav.Key -> ( Model, Cmd Msg )
initModel navKey =
    ( { plants = []
      , navKey = navKey
      }
    , Cmd.none
    )


type Msg
    = SelectPlant Plant


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPlant plant ->
            ( model, Cmd.none )


view : Model -> Html Msg
view _ =
    div []
        [ text "plant list" ]
