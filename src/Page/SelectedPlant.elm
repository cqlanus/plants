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
            [ renderSection selected timing "Timing"
            , renderSection selected growth "Growth"
            , renderSection selected morphology "Morphology"
            , renderSection selected reproduction "Reproduction"
            , renderSection selected chemistry "Chemistry"
            , renderSection selected taxonomy "Taxonomy"
            , renderSection selected status "Status"
            ]
        ]


header : Plant -> Html Msg
header plant =
    h2 [] [ text plant.scientific_name ]


type alias DisplayItem =
    { display : String, value : String }


type alias CategoryStructure =
    Plant -> List DisplayItem


taxonomy : CategoryStructure
taxonomy plant =
    [ { display = "Kingdom", value = plant.kingdom }
    , { display = "Subkingdom", value = plant.subkingdom }
    , { display = "Superdivision", value = plant.superdivision }
    , { display = "Division", value = plant.division }
    , { display = "Subdivision", value = plant.subdivision }
    , { display = "Class", value = plant.class }
    , { display = "Subclass", value = plant.subclass }
    , { display = "Order", value = plant.order }
    , { display = "Family", value = plant.family }
    , { display = "Family Common Name", value = plant.family_common_name }
    , { display = "Genus", value = plant.genus }
    , { display = "Species", value = plant.species }
    , { display = "Subspecies", value = plant.subspecies }
    , { display = "Symbol", value = plant.symbol }
    , { display = "Common Name", value = plant.common_name }
    , { display = "Category", value = plant.category }
    ]


timing : CategoryStructure
timing plant =
    [ { display = "Duration", value = plant.duration }
    , { display = "Active Growth Period", value = plant.active_growth_period }
    , { display = "Bloom Period", value = plant.bloom_period }
    , { display = "Fruit/Seed Start", value = plant.fruit_seed_period_begin }
    , { display = "Fruit/Seed End", value = plant.fruit_seed_period_end }
    , { display = "Lifespan", value = plant.lifespan }
    ]


morphology : CategoryStructure
morphology plant =
    [ { display = "Growth Habit", value = plant.growth_habit }
    , { display = "Growth Form", value = plant.growth_form }
    , { display = "Growth Rate", value = plant.growth_rate }
    , { display = "Shape", value = plant.shape_and_orientation }
    , { display = "Foliage Color", value = plant.foliage_color }
    , { display = "Foliage Texture", value = plant.foliage_texture }
    , { display = "Leaf Retention", value = plant.leaf_retention }
    , { display = "Flower Color", value = plant.flower_color }
    , { display = "Flower Conspicuous", value = plant.flower_conspicuous }
    , { display = "Fall Conspicuous", value = plant.fall_conspicuous }
    , { display = "Fruit Conspicuous", value = plant.fruit_conspicuous }
    , { display = "Mature Height", value = String.fromFloat plant.height_mature_feet }
    ]


reproduction : CategoryStructure
reproduction plant =
    [ { display = "Fertility Requirement", value = plant.fertility_requirement }
    , { display = "Prop By Tubers", value = plant.propogated_by_tubers }
    , { display = "Prop By Sprigs", value = plant.propogated_by_sprigs }
    , { display = "Prop By Sod", value = plant.propogated_by_sod }
    , { display = "Prop By Seed", value = plant.propogated_by_seed }
    , { display = "Prop By Cuttings", value = plant.propogated_by_cuttings }
    , { display = "Prop By Corms", value = plant.propogated_by_corms }
    , { display = "Prop By Container", value = plant.propogated_by_container }
    , { display = "Prop By Bulbs", value = plant.propogated_by_bulbs }
    , { display = "Prop By Bare Root", value = plant.propogated_by_bare_root }
    , { display = "Cold Stratification", value = plant.cold_stratification_required }
    , { display = "Seed Spread Rate", value = plant.seed_spread_rate }
    ]


growth : CategoryStructure
growth plant =
    [ { display = "Min Temp (°F)", value = String.fromFloat plant.temperature_minimum_f }
    , { display = "Precip Max", value = String.fromFloat plant.precipitation_maximum }
    , { display = "Precip Min", value = String.fromFloat plant.precipitation_minimum }
    , { display = "Shade Tolerance", value = plant.shade_tolerance }
    , { display = "Drought Tolerance", value = plant.drought_tolerance }
    , { display = "Moisture Use", value = plant.moisture_use }
    , { display = "Root Depth", value = plant.root_depth_minimum_inches }
    , { display = "Min Frost Free Days", value = String.fromFloat plant.frost_free_days_minimum }
    ]


chemistry : CategoryStructure
chemistry plant =
    [ { display = "C/N Ratio", value = plant.c_n_ratio }
    , { display = "PH Max", value = String.fromFloat plant.ph_maximum }
    , { display = "PH Min", value = String.fromFloat plant.ph_minimum }
    , { display = "CaCO3 Tolerance", value = plant.caco_3_tolerance }
    , { display = "Salinity Tolerance", value = plant.salinity_tolerance }
    , { display = "Nitrogen Fixation", value = plant.nitrogen_fixation }
    , { display = "Anaerobic Tolerance", value = plant.anaerobic_tolerance }
    , { display = "Toxicity", value = plant.toxicity }
    ]


status : CategoryStructure
status plant =
    [ { display = "Native Status", value = plant.native_status }
    , { display = "Invasive", value = plant.invasive }
    , { display = "Federal T/E", value = plant.federal_t_e_status }
    , { display = "State T/E", value = plant.state_t_e_status }
    , { display = "Federal Noxious", value = plant.federal_noxious_status }
    , { display = "State Noxious", value = plant.state_noxious_status }
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
            List.filter (\i -> not (String.isEmpty i.value)) (structure plant)

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
