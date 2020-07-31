module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Styled exposing (toUnstyled)
import Page.Plants as Plants
import Page.Search as Search exposing (update)
import RemoteData
import Route exposing (Route)
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


initialAppState : SharedState
initialAppState =
    { plants = RemoteData.NotAsked }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            , state = initialAppState
            }
    in
    initCurrentPage ( model, Cmd.none )


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
                            Search.initModel model.navKey
                    in
                    ( SearchPage pageModel, Cmd.map SearchPageMsg pageCmds )

                Route.Plants ->
                    let
                        ( pageModel, pageCmds ) =
                            Plants.initModel model.navKey
                    in
                    ( PlantsPage pageModel, Cmd.map PlantsPageMsg pageCmds )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


type Msg
    = SearchPageMsg Search.Msg
    | PlantsPageMsg Plants.Msg
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
            (Search.view >> toUnstyled) pageModel
                |> Html.map SearchPageMsg

        PlantsPage pageModel ->
            Plants.view model.state pageModel
                |> Html.map PlantsPageMsg


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
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( _, _ ) ->
            ( model, Cmd.none )


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
