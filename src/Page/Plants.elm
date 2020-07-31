module Page.Plants exposing (Model, Msg, initModel, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, div, text)
import Plant exposing (Plant)
import RemoteData exposing (WebData)
import SharedState exposing (SharedState)


type alias Model =
    { navKey : Nav.Key
    }


initModel : Nav.Key -> ( Model, Cmd Msg )
initModel navKey =
    ( { navKey = navKey }
    , Cmd.none
    )


type Msg
    = SelectPlant Plant


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectPlant plant ->
            ( model, Cmd.none )


view : SharedState -> Model -> Html Msg
view state _ =
    div []
        [ renderPlants state.plants ]


renderPlants : WebData (List Plant) -> Html Msg
renderPlants resp =
    case resp of
        RemoteData.NotAsked ->
            div [] [ text "Loading..." ]

        RemoteData.Loading ->
            div [] [ text "loading..." ]

        RemoteData.Success plants ->
            div []
                (List.map renderPlant plants)

        RemoteData.Failure _ ->
            div [] [ text "Loading..." ]


renderPlant : Plant -> Html Msg
renderPlant plant =
    div []
        [ text plant.common_name
        , text " | "
        , text plant.genus
        , text " | "
        , text plant.species
        , text " | "
        , text plant.symbol
        ]
