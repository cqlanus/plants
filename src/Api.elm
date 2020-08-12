module Api exposing (ApiResult, base, path)

import Http


base : String
base =
    "http://localhost:9002"


type alias Path =
    { category : String
    , plant : String -> String
    , plantPage : String -> Int -> String
    , guide : String -> String
    }


plantPage : String -> Int -> String
plantPage qs pg =
    base ++ "/plant" ++ qs ++ "&page=" ++ String.fromInt pg


path : Path
path =
    { category = base ++ "/category"
    , plant = \qs -> base ++ "/plant" ++ qs
    , plantPage = plantPage
    , guide = \id -> base ++ "/plant/" ++ id ++ "/guide"
    }


type alias ApiResult a b =
    Result Http.Error a -> b
