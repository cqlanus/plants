module PlantGuide exposing (GuideParagraph, empty, guideDecoder)

import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline as Json


type alias GuideContent =
    { text : String
    , subtitle : String
    }


type alias GuideParagraph =
    { content : GuideContent
    , title : String
    }


guideContentDecoder : Decoder GuideContent
guideContentDecoder =
    Decode.succeed GuideContent
        |> Json.required "text" string
        |> Json.optional "subtitle" string ""


guideDecoder : Decoder GuideParagraph
guideDecoder =
    Decode.succeed GuideParagraph
        |> Json.required "content" guideContentDecoder
        |> Json.optional "title" string ""


empty : List GuideParagraph
empty =
    [ { content = { text = "", subtitle = "" }, title = "" } ]
