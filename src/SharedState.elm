module SharedState exposing (SharedState, SharedStateUpdate(..), update)

import Plant exposing (Plant)
import RemoteData exposing (WebData)


type alias SharedState =
    { plants : WebData (List Plant)
    , plant : Plant.PlantId
    , query : String
    }


type SharedStateUpdate
    = SetPlants (WebData (List Plant)) String
    | SetPlant Plant.PlantId
    | NoUpdate


update : SharedState -> SharedStateUpdate -> SharedState
update state action =
    case action of
        SetPlants plants qs ->
            let
                stuff =
                    Debug.log "plants" plants
            in
            { state | plants = plants, query = qs }

        SetPlant plant ->
            let
                plantId =
                    Debug.log "plant" plant
            in
            { state | plant = plant }

        NoUpdate ->
            state
