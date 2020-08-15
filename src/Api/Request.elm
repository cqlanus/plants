module Api.Request exposing (getCategories, getPlantGuide, getPlants, getPlantsPage)

import Api.Endpoint as Endpoint exposing (Endpoint, unwrap)
import Http
import Json.Decode exposing (Decoder)
import RemoteData exposing (WebData)
import Url.Builder exposing (QueryParameter)


get : Endpoint -> Decoder a -> (WebData a -> b) -> Cmd b
get url decoder msg =
    Http.get
        { url = unwrap url
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        }


getCategories : Decoder a -> (WebData a -> b) -> Cmd b
getCategories decoder msg =
    get Endpoint.category decoder msg


getPlantGuide : String -> Decoder a -> (WebData a -> b) -> Cmd b
getPlantGuide id decoder msg =
    get (Endpoint.guide id) decoder msg


getPlants : List QueryParameter -> Decoder a -> (WebData a -> b) -> Cmd b
getPlants query decoder msg =
    get (Endpoint.plant query) decoder msg


getPlantsPage : Int -> List QueryParameter -> Decoder a -> (WebData a -> b) -> Cmd b
getPlantsPage pageNum query decoder msg =
    let
        pageQ =
            Url.Builder.string "page" (String.fromInt pageNum)

        q =
            List.append query [ pageQ ]
    in
    get (Endpoint.plant q) decoder msg
