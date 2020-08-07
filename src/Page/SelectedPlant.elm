module Page.SelectedPlant exposing (Model, Msg, initModel, update, view)

-- import Html.Styled.Events exposing (onClick)
-- import Route exposing (redirect)

import Browser.Navigation as Nav
import Css exposing (..)
import Html.Styled exposing (Html, div, h2, h3, span, strong, styled, text)
import Plant exposing (Plant, PlantId)
import RemoteData exposing (WebData)
import Route exposing (redirect)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Styled exposing (StyledEl)



-- import Styled exposing (StyledEl)


type alias Model =
    { navKey : Nav.Key }


initModel : Nav.Key -> ( Model, Cmd Msg )
initModel navKey =
    ( { navKey = navKey }, Cmd.none )


type Msg
    = GetPlantGuide Plant


update : Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update msg model =
    case msg of
        GetPlantGuide _ ->
            ( model, Cmd.none, NoUpdate )


filterForPlant : PlantId -> WebData (List Plant) -> List Plant
filterForPlant plantId plants =
    case plants of
        RemoteData.NotAsked ->
            []

        RemoteData.Failure _ ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success pls ->
            List.filter (\p -> p.id == plantId) pls


view : SharedState -> Model -> Html Msg
view { plants, plant } _ =
    let
        filtered =
            filterForPlant plant plants

        selected =
            case List.head filtered of
                Just p ->
                    p

                Nothing ->
                    Plant.empty
    in
    container []
        [ header selected
        , innerContainer []
            [ renderSection selected morphStructure "Morphology"
            , renderSection selected growthStructure "Growth"
            ]
        ]


header : Plant -> Html Msg
header plant =
    h2 [] [ text plant.scientific_name ]


type alias DisplayItem =
    { display : String, value : String }


type alias CategoryStructure =
    Plant -> List DisplayItem


morphStructure : CategoryStructure
morphStructure plant =
    [ { display = "Duration", value = plant.duration }
    , { display = "Growth Habit", value = plant.growth_habit }
    , { display = "Growth Form", value = plant.growth_form }
    , { display = "Growth Rate", value = plant.growth_rate }
    , { display = "Shape", value = plant.shape_and_orientation }
    , { display = "Foliage Color", value = plant.foliage_color }
    , { display = "Foliage Texture", value = plant.foliage_texture }
    , { display = "Leaf Retention", value = plant.leaf_retention }
    , { display = "Flower Color", value = plant.flower_color }
    , { display = "Active Growth Period", value = plant.active_growth_period }
    , { display = "Bloom Period", value = plant.bloom_period }
    ]


reproductionStructure : CategoryStructure
reproductionStructure plant =
    [ { display = "Fertility Requirement", value = plant.fertility_requirement } ]


growthStructure : CategoryStructure
growthStructure plant =
    [ { display = "Min Temp (°F)", value = String.fromFloat plant.temperature_minimum_f }
    , { display = "Precip Max", value = String.fromFloat plant.precipitation_maximum }
    , { display = "Precip Min", value = String.fromFloat plant.precipitation_minimum }
    , { display = "Shade Tolerance", value = plant.shade_tolerance }
    , { display = "Salinity Tolerance", value = plant.salinity_tolerance }
    , { display = "Drought Tolerance", value = plant.drought_tolerance }
    , { display = "Moisture User", value = plant.moisture_use }
    , { display = "PH Max", value = String.fromFloat plant.ph_maximum }
    , { display = "PH Min", value = String.fromFloat plant.ph_minimum }
    , { display = "Root Depth", value = plant.root_depth_minimum_inches }
    ]


renderRow : List DisplayItem -> List (Html Msg)
renderRow items =
    List.map
        (\i ->
            div []
                [ strong [] [ text (i.display ++ ": ") ]
                , span [] [ text i.value ]
                ]
        )
        items


renderSection : Plant -> CategoryStructure -> String -> Html Msg
renderSection plant structure name =
    let
        structureList =
            structure plant

        title =
            h3 [] [ text name ]
    in
    sectionContainer []
        (title :: renderRow structureList)


container : StyledEl div
container =
    styled div
        [ padding2 (rem 0) (rem 1) ]


innerContainer : StyledEl div
innerContainer =
    styled div
        [ displayFlex
        , flexWrap wrap
        ]


sectionContainer : StyledEl div
sectionContainer =
    styled div
        [ textAlign left
        , flex (num 1)
        , flexBasis (rem 15)
        ]
