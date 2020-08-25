module Api.Endpoint exposing (Endpoint, category, getBase, guide, images, plant, plantId, unwrap, url)

import Url.Builder exposing (QueryParameter)


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


getBase : Bool -> String
getBase isDev =
    if isDev then
        "http://localhost:9001"

    else
        "https://plants-api.chrislanus.com"


base : String
base =
    "http://localhost:9001"


url : Bool -> List String -> List QueryParameter -> Endpoint
url isDev paths queryParams =
    Url.Builder.crossOrigin (getBase isDev)
        paths
        queryParams
        |> Endpoint


category : Bool -> Endpoint
category isDev =
    url isDev [ "category" ] []


plant : Bool -> List QueryParameter -> Endpoint
plant isDev query =
    url isDev [ "plant" ] query


plantId : Bool -> String -> Endpoint
plantId isDev id =
    url isDev [ "plant" ] [ Url.Builder.string "id" id ]


guide : Bool -> String -> Endpoint
guide isDev id =
    url isDev [ "plant", id, "guide" ] []


images : Bool -> String -> Endpoint
images isDev id =
    url isDev [ "plant", id, "images" ] []
