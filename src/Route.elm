module Route exposing (Route(..), parseUrl, pushUrl, redirect)

import Browser.Navigation as Nav
import Plant exposing (PlantId)
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, top)


type Route
    = NotFound
    | Plants
    | Plant PlantId
    | Search


parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ map Search top
        , map Search (s "search")
        , map Plants (s "plants")
        , map Plant (s "plants" </> Plant.idParser)
        ]


pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey


redirect : Route -> Route -> Nav.Key -> Cmd msg
redirect toRoute currentRoute navKey =
    if currentRoute /= toRoute then
        routeToString toRoute
            |> Nav.replaceUrl navKey

    else
        Cmd.none


routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        Plants ->
            "/plants"

        Plant plantId ->
            "/plants/" ++ Plant.idToString plantId

        Search ->
            "/search"
