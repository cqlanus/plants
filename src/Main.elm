module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (Html, h3, text)
import Html.Styled exposing (toUnstyled)
import Page.Plants as Plants
import Page.Search as Search exposing (update)
import Page.SelectedPlant as Selected
import Plant exposing (PlantId(..), PlantsResponse, intToPlantId, plantIdToInt)
import RemoteData exposing (WebData)
import Route exposing (Route(..), redirect)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Url exposing (Url)


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    , state : SharedState
    }


type Page
    = NotFoundPage
    | SearchPage Search.Model
    | PlantsPage Plants.Model
    | SelectedPage Selected.Model


initialAppState : SharedState
initialAppState =
    { plants = RemoteData.NotAsked
    , query = []
    , plant = intToPlantId 0
    , plantGuide = RemoteData.NotAsked
    , fieldCategories = RemoteData.NotAsked
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        current =
            Route.parseUrl url

        ( route, cmd ) =
            getRoute initialAppState.plants current navKey url

        model =
            { route = route
            , page = NotFoundPage
            , navKey = navKey
            , state = initialAppState
            }
    in
    initCurrentPage ( model, cmd )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.Search ->
                    let
                        ( pageModel, pageCmds ) =
                            Search.initModel model.navKey model.state
                    in
                    ( SearchPage pageModel, Cmd.map SearchPageMsg pageCmds )

                Route.Plants ->
                    let
                        ( pageModel, pageCmds ) =
                            Plants.initModel model.navKey
                    in
                    ( PlantsPage pageModel, Cmd.map PlantsPageMsg pageCmds )

                Route.Plant id ->
                    let
                        ( pageModel, pageCmds ) =
                            Selected.initModel model.navKey model.state id
                    in
                    ( SelectedPage pageModel, Cmd.map SelectedPageMsg pageCmds )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


type Msg
    = SearchPageMsg Search.Msg
    | PlantsPageMsg Plants.Msg
    | SelectedPageMsg Selected.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url


view : Model -> Document Msg
view model =
    { title = "Plants"
    , body = [ currentView model ]
    }


currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        SearchPage pageModel ->
            (Search.view model.state >> toUnstyled) pageModel
                |> Html.map SearchPageMsg

        PlantsPage pageModel ->
            (Plants.view model.state >> toUnstyled) pageModel
                |> Html.map PlantsPageMsg

        SelectedPage pageModel ->
            (Selected.view model.state >> toUnstyled) pageModel
                |> Html.map SelectedPageMsg


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Could not find page" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( SearchPageMsg subMsg, SearchPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd, newSharedStateUpdate ) =
                    Search.update subMsg pageModel

                nextSharedState =
                    SharedState.update model.state newSharedStateUpdate
            in
            ( { model | page = SearchPage updatedPageModel, state = nextSharedState }
            , Cmd.map SearchPageMsg updatedCmd
            )

        ( PlantsPageMsg subMsg, PlantsPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd, newSharedStateUpdate ) =
                    Plants.update subMsg pageModel model.state

                nextSharedState =
                    SharedState.update
                        model.state
                        newSharedStateUpdate
            in
            ( { model | page = PlantsPage updatedPageModel, state = nextSharedState }
            , Cmd.map PlantsPageMsg updatedCmd
            )

        ( SelectedPageMsg subMsg, SelectedPage pageModel ) ->
            let
                id =
                    case model.route of
                        NotFound ->
                            PlantId 0

                        Plants ->
                            PlantId 0

                        Search ->
                            PlantId 0

                        Plant pId ->
                            pId

                ( updatedPageModel, updatedCmd, newSharedStateUpdate ) =
                    Selected.update subMsg pageModel model.state id

                nextSharedState =
                    SharedState.update model.state newSharedStateUpdate
            in
            ( { model | page = SelectedPage updatedPageModel, state = nextSharedState }
            , Cmd.map SelectedPageMsg updatedCmd
            )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                ( newRoute, cmd ) =
                    getRoute model.state.plants model.route model.navKey url
            in
            ( { model | route = newRoute }, cmd )
                |> initCurrentPage

        ( _, _ ) ->
            ( model, Cmd.none )


getRoute : WebData PlantsResponse -> Route -> Nav.Key -> Url -> ( Route, Cmd Msg )
getRoute plants currentRoute navKey url =
    case plants of
        RemoteData.NotAsked ->
            let
                dest =
                    case currentRoute of
                        Route.Search ->
                            Route.Search

                        Route.Plants ->
                            Route.Search

                        Route.NotFound ->
                            Route.Search

                        Route.Plant id ->
                            Route.Plant id
            in
            ( dest, Route.redirect Route.Search currentRoute navKey )

        RemoteData.Loading ->
            ( Route.parseUrl url, Cmd.none )

        RemoteData.Success _ ->
            ( Route.parseUrl url, Cmd.none )

        RemoteData.Failure _ ->
            ( Route.parseUrl url, Cmd.none )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
