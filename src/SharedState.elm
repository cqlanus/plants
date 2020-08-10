module SharedState exposing (SharedState, SharedStateUpdate(..), update)

import Plant exposing (Plant)
import PlantGuide exposing (GuideParagraph)
import RemoteData exposing (WebData)


type alias SharedState =
    { plants : WebData (List Plant)
    , plant : Plant.PlantId
    , plantGuide : WebData (List GuideParagraph)
    , query : String
    }


type SharedStateUpdate
    = SetPlants (WebData (List Plant)) String
    | SetPlant Plant.PlantId
    | SetPlantGuide (WebData (List GuideParagraph))
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

        NoUpdate ->
            state
