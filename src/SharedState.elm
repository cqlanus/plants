module SharedState exposing (SharedState, SharedStateUpdate(..), getInitial, update)

import Field exposing (FieldCategory)
import Plant exposing (PlantId, PlantsResponse)
import PlantGuide exposing (GuideParagraph)
import RemoteData exposing (WebData)
import Url.Builder exposing (QueryParameter)


type alias Flags =
    { env : String
    }


type alias SharedState =
    { plants : WebData PlantsResponse
    , plant : Plant.PlantId
    , plantGuide : WebData (List GuideParagraph)
    , fieldCategories : WebData (List FieldCategory)
    , query : List QueryParameter
    , flags : Flags
    }


type SharedStateUpdate
    = SetPlants (WebData PlantsResponse) (List QueryParameter)
    | SetPlant Plant.PlantId
    | LoadPlant (WebData PlantsResponse) (List QueryParameter) PlantId
    | SetPlantGuide (WebData (List GuideParagraph))
    | SetCategories (WebData (List FieldCategory))
    | NoUpdate


update : SharedState -> SharedStateUpdate -> SharedState
update state action =
    case action of
        SetPlants plants qs ->
            { state | plants = plants, query = qs }

        LoadPlant plants qs id ->
            { state | plants = plants, query = qs, plant = id }

        SetPlant plant ->
            { state | plant = plant, plantGuide = RemoteData.NotAsked }

        SetPlantGuide guide ->
            { state | plantGuide = guide }

        SetCategories response ->
            { state | fieldCategories = response }

        NoUpdate ->
            state


getInitial : SharedState -> { env : String } -> SharedState
getInitial state flags =
    { state | flags = flags }
