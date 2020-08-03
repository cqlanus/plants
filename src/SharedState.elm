module SharedState exposing (SharedState, SharedStateUpdate(..), update)

import Plant exposing (Plant)
import RemoteData exposing (WebData)


type alias SharedState =
    { plants : WebData (List Plant)
    }


type SharedStateUpdate
    = SetPlants (WebData (List Plant))
    | NoUpdate


update : SharedState -> SharedStateUpdate -> SharedState
update state action =
    case action of
        SetPlants plants ->
            let
                stuff =
                    Debug.log "plants" plants
            in
            { state | plants = plants }

        NoUpdate ->
            state
