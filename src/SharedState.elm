module SharedState exposing (SharedState, SharedStateUpdate(..), update)

import Field exposing (FieldCategory)
import Plant exposing (PlantsResponse)
import PlantGuide exposing (GuideParagraph)
import RemoteData exposing (WebData)
import Url.Builder exposing (QueryParameter)


type alias SharedState =
    { plants : WebData PlantsResponse
    , plant : Plant.PlantId
    , plantGuide : WebData (List GuideParagraph)
    , fieldCategories : WebData (List FieldCategory)
    , query : List QueryParameter
    }


type SharedStateUpdate
    = SetPlants (WebData PlantsResponse) (List QueryParameter)
    | SetPlant Plant.PlantId
    | SetPlantGuide (WebData (List GuideParagraph))
    | SetCategories (WebData (List FieldCategory))
    | NoUpdate


update : SharedState -> SharedStateUpdate -> SharedState
update state action =
    case action of
        SetPlants plants qs ->
            { state | plants = plants, query = qs }

        SetPlant plant ->
            { state | plant = plant, plantGuide = RemoteData.NotAsked }

        SetPlantGuide guide ->
            { state | plantGuide = guide }

        SetCategories response ->
            { state | fieldCategories = response }

        NoUpdate ->
            state
