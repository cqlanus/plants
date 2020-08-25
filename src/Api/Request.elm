module Api.Request exposing (getCategories, getPlant, getPlantGuide, getPlantImages, getPlants, getPlantsPage)

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


getCategories : Bool -> Decoder a -> (WebData a -> b) -> Cmd b
getCategories isDev decoder msg =
    get (Endpoint.category isDev) decoder msg


getPlantGuide : Bool -> String -> Decoder a -> (WebData a -> b) -> Cmd b
getPlantGuide isDev id decoder msg =
    get (Endpoint.guide isDev id) decoder msg


getPlant : Bool -> String -> Decoder a -> (WebData a -> b) -> Cmd b
getPlant isDev id decoder msg =
    get (Endpoint.plantId isDev id) decoder msg


getPlantImages : Bool -> String -> Decoder a -> (WebData a -> b) -> Cmd b
getPlantImages isDev id decoder msg =
    get (Endpoint.images isDev id) decoder msg


getPlants : Bool -> List QueryParameter -> Decoder a -> (WebData a -> b) -> Cmd b
getPlants isDev query decoder msg =
    get (Endpoint.plant isDev query) decoder msg


getPlantsPage : Bool -> Int -> List QueryParameter -> Decoder a -> (WebData a -> b) -> Cmd b
getPlantsPage isDev pageNum query decoder msg =
    let
        pageQ =
            Url.Builder.string "page" (String.fromInt pageNum)

        q =
            List.append query [ pageQ ]
    in
    get (Endpoint.plant isDev q) decoder msg
