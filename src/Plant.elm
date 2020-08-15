module Plant exposing (Plant, PlantId, PlantsResponse, empty, getPlantsDecoder, idParser, idToString, intToPlantId, plantDecoder, plantIdToInt, plantImagesDecoder)

import Json.Decode as Decode exposing (Decoder, float, int, list, string)
import Json.Decode.Pipeline as Json exposing (required)
import Url.Parser exposing (Parser, custom)


type PlantId
    = PlantId Int


intToPlantId : Int -> PlantId
intToPlantId num =
    PlantId num


plantIdDecoder : Decoder PlantId
plantIdDecoder =
    Decode.map PlantId int


plantIdToInt : PlantId -> Int
plantIdToInt plantId =
    case plantId of
        PlantId id ->
            id


idParser : Parser (PlantId -> a) a
idParser =
    custom "PLANTID" <|
        \plantId ->
            Maybe.map PlantId (String.toInt plantId)


idToString : PlantId -> String
idToString plantId =
    String.fromInt (plantIdToInt plantId)


type alias Plant =
    { id : PlantId
    , veneer_product : String
    , pulpwood_product : String
    , protein_potential : String
    , post_product : String
    , palatable_human : String
    , palatable_graze_animal : String
    , palatable_browse_animal : String
    , nursery_stock_product : String
    , naval_store_product : String
    , lumber_product : String
    , fuelwood_product : String
    , fodder_product : String
    , christmas_tree_product : String
    , berry_nut_seed_product : String
    , vegetative_spread_rate : String
    , small_grain : String
    , seedling_vigor : String
    , seed_spread_rate : String
    , seeds_per_pound : Float
    , propogated_by_tubers : String
    , propogated_by_sprigs : String
    , propogated_by_sod : String
    , propogated_by_seed : String
    , propogated_by_cuttings : String
    , propogated_by_corms : String
    , propogated_by_container : String
    , propogated_by_bulbs : String
    , propogated_by_bare_root : String
    , fruit_seed_persistence : String
    , fruit_seed_period_end : String
    , fruit_seed_period_begin : String
    , fruit_seed_abundance : String
    , commercial_availability : String
    , bloom_period : String
    , temperature_minimum_f : Float
    , shade_tolerance : String
    , salinity_tolerance : String
    , root_depth_minimum_inches : String
    , precipitation_maximum : Float
    , precipitation_minimum : Float
    , planting_density_per_acre_maximum : Float
    , planting_density_per_acre_minimum : Float
    , ph_maximum : Float
    , ph_minimum : Float
    , moisture_use : String
    , hedge_tolerance : String
    , frost_free_days_minimum : Float
    , fire_tolerance : String
    , fertility_requirement : String
    , drought_tolerance : String
    , cold_stratification_required : String
    , caco_3_tolerance : String
    , anaerobic_tolerance : String
    , adapted_to_fine_textured_soils : String
    , adapted_to_medium_textured_soils : String
    , adapted_to_coarse_textured_soils : String
    , toxicity : String
    , shape_and_orientation : String
    , resprout_ability : String
    , nitrogen_fixation : String
    , low_growing_grass : String
    , lifespan : String
    , leaf_retention : String
    , known_allelopath : String
    , height_mature_feet : Float
    , height_at_base_age_maximum_feet : Float
    , growth_rate : String
    , growth_form : String
    , fruit_conspicuous : String
    , fruit_color : String
    , foliage_texture : String
    , foliage_porosity_winter : String
    , foliage_porosity_summer : String
    , foliage_color : String
    , flower_conspicuous : String
    , flower_color : String
    , fire_resistance : String
    , fall_conspicuous : String
    , coppice_potential : String
    , c_n_ratio : String
    , bloat : String
    , after_harvest_regrowth_rate : String
    , active_growth_period : String
    , cultivar_name : String
    , characteristics_data : String
    , plant_guides : String
    , plant_guides_text : String
    , fact_sheets : String
    , image_gallery : String
    , regional_wetland_indicator_status : String
    , national_wetland_indicator_status : String
    , state_t_e_common_name : String
    , state_t_e_status : String
    , federal_t_e_status : String
    , invasive : String
    , state_noxious_common_name : String
    , state_noxious_status : String
    , federal_noxious_common_name : String
    , federal_noxious_status : String
    , native_status : String
    , growth_habit : String
    , duration : String
    , itis_tsn : String
    , kingdom : String
    , subkingdom : String
    , superdivision : String
    , division : String
    , subdivision : String
    , class : String
    , subclass : String
    , order : String
    , family_common_name : String
    , family_symbol : String
    , family : String
    , genus : String
    , category : String
    , state_and_province : String
    , common_name : String
    , parents : String
    , questionable_taxon_indicator : String
    , quadranomial_author : String
    , trinomial_author : String
    , genera_binomial_author : String
    , forma : String
    , forma_prefix : String
    , subvariety : String
    , subvariety_prefix : String
    , variety : String
    , hybrid_variety_indicator : String
    , variety_prefix : String
    , subspecies : String
    , hybrid_subspecies_indicator : String
    , subspecies_prefix : String
    , species : String
    , hybrid_species_indicator : String
    , hybrid_genus_indicator : String
    , scientific_name : String
    , symbol : String
    , synonym_symbol : String
    , accepted_symbol : String
    }


type alias PlantsResponse =
    { rows : List Plant
    , count : Int
    }


plantImagesDecoder : Decoder (List String)
plantImagesDecoder =
    list string


getPlantsDecoder : Decoder PlantsResponse
getPlantsDecoder =
    Decode.succeed PlantsResponse
        |> Json.required "rows" (list plantDecoder)
        |> Json.required "count" int


plantDecoder : Decoder Plant
plantDecoder =
    Decode.succeed Plant
        |> Json.required "id" plantIdDecoder
        |> Json.optional "veneer_product" string ""
        |> Json.optional "pulpwood_product" string ""
        |> Json.optional "protein_potential" string ""
        |> Json.optional "post_product" string ""
        |> Json.optional "palatable_human" string ""
        |> Json.optional "palatable_graze_animal" string ""
        |> Json.optional "palatable_browse_animal" string ""
        |> Json.optional "nursery_stock_product" string ""
        |> Json.optional "naval_store_product" string ""
        |> Json.optional "lumber_product" string ""
        |> Json.optional "fuelwood_product" string ""
        |> Json.optional "fodder_product" string ""
        |> Json.optional "christmas_tree_product" string ""
        |> Json.optional "berry_nut_seed_product" string ""
        |> Json.required "vegetative_spread_rate" string
        |> Json.required "small_grain" string
        |> Json.required "seedling_vigor" string
        |> Json.required "seed_spread_rate" string
        |> Json.required "seeds_per_pound" float
        |> Json.required "propogated_by_tubers" string
        |> Json.required "propogated_by_sprigs" string
        |> Json.required "propogated_by_sod" string
        |> Json.required "propogated_by_seed" string
        |> Json.required "propogated_by_cuttings" string
        |> Json.required "propogated_by_corms" string
        |> Json.required "propogated_by_container" string
        |> Json.required "propogated_by_bulbs" string
        |> Json.required "propogated_by_bare_root" string
        |> Json.required "fruit_seed_persistence" string
        |> Json.required "fruit_seed_period_end" string
        |> Json.required "fruit_seed_period_begin" string
        |> Json.required "fruit_seed_abundance" string
        |> Json.required "commercial_availability" string
        |> Json.required "bloom_period" string
        |> Json.required "temperature_minimum_f" float
        |> Json.required "shade_tolerance" string
        |> Json.required "salinity_tolerance" string
        |> Json.required "root_depth_minimum_inches" string
        |> Json.required "precipitation_maximum" float
        |> Json.required "precipitation_minimum" float
        |> Json.required "planting_density_per_acre_maximum" float
        |> Json.required "planting_density_per_acre_minimum" float
        |> Json.required "ph_maximum" float
        |> Json.required "ph_minimum" float
        |> Json.required "moisture_use" string
        |> Json.required "hedge_tolerance" string
        |> Json.required "frost_free_days_minimum" float
        |> Json.required "fire_tolerance" string
        |> Json.required "fertility_requirement" string
        |> Json.required "drought_tolerance" string
        |> Json.required "cold_stratification_required" string
        |> Json.required "caco_3_tolerance" string
        |> Json.required "anaerobic_tolerance" string
        |> Json.required "adapted_to_fine_textured_soils" string
        |> Json.required "adapted_to_medium_textured_soils" string
        |> Json.required "adapted_to_coarse_textured_soils" string
        |> Json.required "toxicity" string
        |> Json.required "shape_and_orientation" string
        |> Json.required "resprout_ability" string
        |> Json.required "nitrogen_fixation" string
        |> Json.required "low_growing_grass" string
        |> Json.required "lifespan" string
        |> Json.required "leaf_retention" string
        |> Json.required "known_allelopath" string
        |> Json.required "height_mature_feet" float
        |> Json.required "height_at_base_age_maximum_feet" float
        |> Json.required "growth_rate" string
        |> Json.required "growth_form" string
        |> Json.required "fruit_conspicuous" string
        |> Json.required "fruit_color" string
        |> Json.required "foliage_texture" string
        |> Json.required "foliage_porosity_winter" string
        |> Json.required "foliage_porosity_summer" string
        |> Json.required "foliage_color" string
        |> Json.required "flower_conspicuous" string
        |> Json.required "flower_color" string
        |> Json.required "fire_resistance" string
        |> Json.required "fall_conspicuous" string
        |> Json.required "coppice_potential" string
        |> Json.required "c_n_ratio" string
        |> Json.required "bloat" string
        |> Json.required "after_harvest_regrowth_rate" string
        |> Json.required "active_growth_period" string
        |> Json.optional "cultivar_name" string ""
        |> Json.required "characteristics_data" string
        |> Json.required "plant_guides" string
        |> Json.optional "plant_guides_test" string ""
        |> Json.required "fact_sheets" string
        |> Json.required "image_gallery" string
        |> Json.required "regional_wetland_indicator_status" string
        |> Json.required "national_wetland_indicator_status" string
        |> Json.required "state_t_e_common_name" string
        |> Json.required "state_t_e_status" string
        |> Json.required "federal_t_e_status" string
        |> Json.required "invasive" string
        |> Json.required "state_noxious_common_name" string
        |> Json.required "state_noxious_status" string
        |> Json.required "federal_noxious_common_name" string
        |> Json.required "federal_noxious_status" string
        |> Json.required "native_status" string
        |> Json.required "growth_habit" string
        |> Json.required "duration" string
        |> Json.required "itis_tsn" string
        |> Json.required "kingdom" string
        |> Json.required "subkingdom" string
        |> Json.required "superdivision" string
        |> Json.required "division" string
        |> Json.required "subdivision" string
        |> Json.required "class" string
        |> Json.required "subclass" string
        |> Json.required "order" string
        |> Json.required "family_common_name" string
        |> Json.required "family_symbol" string
        |> Json.required "family" string
        |> Json.required "genus" string
        |> Json.required "category" string
        |> Json.required "state_and_province" string
        |> Json.required "common_name" string
        |> Json.required "parents" string
        |> Json.required "questionable_taxon_indicator" string
        |> Json.required "quadranomial_author" string
        |> Json.required "trinomial_author" string
        |> Json.required "genera_binomial_author" string
        |> Json.required "forma" string
        |> Json.required "forma_prefix" string
        |> Json.required "subvariety" string
        |> Json.required "subvariety_prefix" string
        |> Json.required "variety" string
        |> Json.required "hybrid_variety_indicator" string
        |> Json.required "variety_prefix" string
        |> Json.required "subspecies" string
        |> Json.required "hybrid_subspecies_indicator" string
        |> Json.required "subspecies_prefix" string
        |> Json.required "species" string
        |> Json.required "hybrid_species_indicator" string
        |> Json.required "hybrid_genus_indicator" string
        |> Json.required "scientific_name" string
        |> Json.required "symbol" string
        |> Json.required "synonym_symbol" string
        |> Json.required "accepted_symbol" string


empty : Plant
empty =
    Plant
        (PlantId 0)
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        0
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        0
        ""
        ""
        ""
        0
        0
        0
        0
        0
        0
        ""
        ""
        0
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        0
        0
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
        ""
