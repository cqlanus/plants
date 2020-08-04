module Page.SelectedPlant exposing (Model, Msg, initModel, update, view)

-- import Html.Styled.Events exposing (onClick)
-- import Route exposing (redirect)

import Browser.Navigation as Nav
import Css exposing (..)
import Html.Styled exposing (Html, div)
import Plant exposing (Plant, PlantId)
import RemoteData exposing (WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))



-- import Styled exposing (StyledEl)


type alias Model =
    { navKey : Nav.Key }


initModel : Nav.Key -> ( Model, Cmd Msg )
initModel navKey =
    ( { navKey = navKey }, Cmd.none )


type Msg
    = GetPlantGuide Plant


update : Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update msg model =
    case msg of
        GetPlantGuide _ ->
            ( model, Cmd.none, NoUpdate )


filterForPlant : PlantId -> WebData (List Plant) -> List Plant
filterForPlant plantId plants =
    case plants of
        RemoteData.NotAsked ->
            []

        RemoteData.Failure _ ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success pls ->
            List.filter (\p -> p.id == plantId) pls


view : SharedState -> Model -> Html Msg
view { plants, plant } _ =
    let
        selected =
            filterForPlant plant plants

        test_ =
            Debug.log "selected" selected
    in
    div [] []
