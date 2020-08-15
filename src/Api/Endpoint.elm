module Api.Endpoint exposing (Endpoint, category, guide, plant, unwrap, url)

import Url.Builder exposing (QueryParameter)


type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


base : String
base =
    "http://localhost:9001"


url : List String -> List QueryParameter -> Endpoint
url paths queryParams =
    Url.Builder.crossOrigin base
        paths
        queryParams
        |> Endpoint


category : Endpoint
category =
    url [ "category" ] []


plant : List QueryParameter -> Endpoint
plant query =
    url [ "plant" ] query


guide : String -> Endpoint
guide id =
    url [ "plant", id, "guide" ] []
