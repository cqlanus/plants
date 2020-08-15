module Page.SelectedPlant exposing (Model, Msg, initModel, update, view)

import Api.Request as Request
import Array exposing (fromList)
import Browser.Navigation as Nav
import Css exposing (..)
import Html.Styled exposing (Html, div, em, h2, h3, img, p, span, strong, styled, text)
import Html.Styled.Attributes exposing (attribute, css, src)
import Html.Styled.Events exposing (onClick)
import Json.Decode exposing (list)
import Pagination exposing (pageText, pagingButton, pagingContainer)
import Plant exposing (Plant, PlantId, PlantsResponse, plantIdToInt, plantImagesDecoder)
import PlantGuide exposing (GuideParagraph, guideDecoder)
import RemoteData exposing (WebData)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Styled exposing (StyledEl)


type alias Model =
    { navKey : Nav.Key
    , images : WebData (List String)
    , imgIdx : Int
    }


initModel : Nav.Key -> SharedState -> ( Model, Cmd Msg )
initModel navKey state =
    ( { navKey = navKey
      , images = RemoteData.NotAsked
      , imgIdx = 0
      }
    , getPlantImages state
    )


type Msg
    = GetPlantGuide Plant
    | HandlePlantGuide (WebData (List GuideParagraph))
    | HandlePlantImages (WebData (List String))
    | SelectImage Int


getPlantImages : SharedState -> Cmd Msg
getPlantImages state =
    let
        plantId =
            String.fromInt (plantIdToInt state.plant)
    in
    Request.getPlantImages plantId plantImagesDecoder HandlePlantImages


fetchPlantGuide : Plant -> Cmd Msg
fetchPlantGuide plant =
    let
        id =
            String.fromInt (plantIdToInt plant.id)
    in
    Request.getPlantGuide id (list guideDecoder) HandlePlantGuide


update : Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update msg model =
    case msg of
        GetPlantGuide plant ->
            ( model, fetchPlantGuide plant, NoUpdate )

        HandlePlantGuide guide ->
            ( model, Cmd.none, SetPlantGuide guide )

        HandlePlantImages images ->
            ( { model | images = images }, Cmd.none, NoUpdate )

        SelectImage num ->
            ( { model | imgIdx = num }, Cmd.none, NoUpdate )


filterForPlant : PlantId -> WebData PlantsResponse -> List Plant
filterForPlant plantId plants =
    case plants of
        RemoteData.NotAsked ->
            []

        RemoteData.Failure _ ->
            []

        RemoteData.Loading ->
            []

        RemoteData.Success resp ->
            List.filter (\p -> p.id == plantId) resp.rows


view : SharedState -> Model -> Html Msg
view { plants, plant, plantGuide } model =
    let
        filtered =
            filterForPlant plant plants

        selected =
            case List.head filtered of
                Just p ->
                    p

                Nothing ->
                    Plant.empty

        renderFor =
            renderSection selected
    in
    container []
        [ header selected
        , innerContainer []
            [ renderImage model ]
        , styledH3 [] [ text "Characteristics" ]
        , innerContainer []
            [ renderFor timing "Timing"
            , renderFor growth "Growth"
            , renderFor morphology "Morphology"
            , renderFor reproduction "Reproduction"
            , renderFor chemistry "Chemistry"
            , renderFor taxonomy "Taxonomy"
            , renderFor status "Status"
            ]
        , div []
            [ renderButton selected
            , renderGuide plantGuide
            ]
        ]


header : Plant -> Html Msg
header plant =
    div []
        [ h2 [] [ text plant.scientific_name ]
        , Html.Styled.em [] [ text plant.common_name ]
        ]


type alias DisplayItem =
    { display : String
    , value : String
    }


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


renderImage : Model -> Html Msg
renderImage model =
    let
        empty =
            div [] []

        currentIdx =
            model.imgIdx

        image =
            case model.images of
                RemoteData.NotAsked ->
                    empty

                RemoteData.Failure _ ->
                    empty

                RemoteData.Loading ->
                    empty

                RemoteData.Success images ->
                    let
                        current =
                            Array.get currentIdx (fromList images)

                        length =
                            List.length images

                        last =
                            length - 1

                        imgText =
                            String.fromInt (currentIdx + 1) ++ " / " ++ String.fromInt length

                        dec =
                            if currentIdx == 0 then
                                0

                            else
                                currentIdx - 1

                        inc =
                            if currentIdx == last then
                                last

                            else
                                currentIdx + 1
                    in
                    case current of
                        Just i ->
                            div [ css [ width (pct 100) ] ]
                                [ imageContainer [] [ img [ src i ] [] ]
                                , pagingContainer []
                                    [ pagingButton [ onClick (SelectImage dec) ] [ text "<" ]
                                    , pageText [] [ text imgText ]
                                    , pagingButton [ onClick (SelectImage inc) ] [ text ">" ]
                                    ]
                                ]

                        Nothing ->
                            div [] []
    in
    image


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


renderButton : Plant -> Html Msg
renderButton plant =
    let
        shouldRender =
            not (String.isEmpty plant.plant_guides)
    in
    if shouldRender then
        styledH3 [ onClick (GetPlantGuide plant), attribute "role" "button" ] [ text "Plant Guide" ]

    else
        span [] []


renderGuide : WebData (List GuideParagraph) -> Html Msg
renderGuide data =
    case data of
        RemoteData.NotAsked ->
            div [] []

        RemoteData.Loading ->
            div [] [ text "..." ]

        RemoteData.Failure _ ->
            div [] [ text "something went wrong" ]

        RemoteData.Success guide ->
            let
                paras =
                    List.map (\p -> renderGuideParagraph p) guide
            in
            div [] paras


renderGuideParagraph : GuideParagraph -> Html Msg
renderGuideParagraph para =
    let
        title =
            if String.isEmpty para.title then
                span [] []

            else
                h3 [] [ text para.title ]

        contentText =
            p [] [ text para.content.text ]

        subtitle =
            if String.isEmpty para.content.subtitle then
                span [] []

            else
                strong [] [ text para.content.subtitle ]
    in
    div []
        [ title
        , subtitle
        , contentText
        ]


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


imageContainer : StyledEl div
imageContainer =
    styled div
        [ flex (num 1) ]


styledH3 : StyledEl h3
styledH3 =
    styled h3
        [ cursor pointer
        , hover
            [ fontWeight bold ]
        ]
