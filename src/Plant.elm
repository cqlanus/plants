module Plant exposing (Plant, PlantId, plantDecoder)

import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Pipeline as Json exposing (required)


type PlantId
    = PlantId Int


type alias Plant =
    { id : PlantId
    , species : String
    , genus : String
    , common_name : String
    , symbol : String
    }


plantDecoder : Decoder Plant
plantDecoder =
    Decode.succeed Plant
        |> Json.required "id" plantIdDecoder
        |> Json.required "genus" string
        |> Json.required "species" string
        |> Json.required "common_name" string
        |> Json.required "symbol" string


plantIdDecoder : Decoder PlantId
plantIdDecoder =
    Decode.map PlantId int
