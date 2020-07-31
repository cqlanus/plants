module Field exposing (Field, FieldCategory, FieldCriteria, FieldData, FieldOption, categoryDecoder)

import Json.Decode as Decode exposing (Decoder, list, string)
import Json.Decode.Pipeline as Json exposing (required)


type alias FieldOption =
    { display : String
    , value : String
    }


type alias Field =
    { title : String
    , options : List FieldOption
    , key : String
    }


type alias FieldCriteria =
    List Field


type alias FieldCategory =
    { name : String
    , criteria : FieldCriteria
    }


type alias FieldData =
    { field : Field
    , value : Maybe String
    }


optionDecoder : Decoder FieldOption
optionDecoder =
    Decode.succeed FieldOption
        |> Json.required "display" string
        |> Json.required "value" string


criteriaDecoder : Decoder Field
criteriaDecoder =
    Decode.succeed Field
        |> Json.required "key" string
        |> Json.required "options" (list optionDecoder)
        |> Json.required "title" string


categoryDecoder : Decoder FieldCategory
categoryDecoder =
    Decode.succeed FieldCategory
        |> Json.required "name" string
        |> Json.required "criteria" (list criteriaDecoder)
