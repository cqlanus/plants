module Page.Plants exposing (Model, Msg, initModel, update, view)

import Api.Request as Request
import Basics as B
import Browser.Navigation as Nav
import Css exposing (..)
import Html.Styled exposing (Html, button, div, em, h3, span, strong, styled, text)
import Html.Styled.Attributes exposing (attribute)
import Html.Styled.Events exposing (onClick)
import Pagination exposing (pageText, pagingButton, pagingContainer)
import Plant exposing (Plant, PlantsResponse, getPlantsDecoder, plantIdToInt, plantImagesDecoder)
import RemoteData exposing (WebData)
import Route
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Styled exposing (StyledEl)
import Url.Builder exposing (QueryParameter, toQuery)


type alias Emoji =
    { book : String
    , chart : String
    , sheet : String
    , image : String
    }


emoji : Emoji
emoji =
    { book = "📖"
    , chart = "📊"
    , sheet = "📝"
    , image = "🖼️"
    }


type alias Model =
    { navKey : Nav.Key
    , page : Int
    }


initModel : Nav.Key -> ( Model, Cmd Msg )
initModel navKey =
    ( { navKey = navKey
      , page = 1
      }
    , Cmd.none
    )


getPlants : Int -> SharedState -> Cmd Msg
getPlants pageNum state =
    let
        isDev =
            state.flags.env == "dev"
    in
    Request.getPlantsPage isDev pageNum state.query getPlantsDecoder (HandleNextPage state.query)


type Msg
    = SelectPlant Plant
    | SetPage Int
    | HandleNextPage (List QueryParameter) (WebData PlantsResponse)


update : Msg -> Model -> SharedState -> ( Model, Cmd Msg, SharedStateUpdate )
update msg model state =
    case msg of
        SelectPlant plant ->
            ( model, Route.pushUrl (Route.Plant plant.id) model.navKey, SetPlant plant.id )

        SetPage pageNum ->
            ( { model | page = pageNum }, getPlants pageNum state, NoUpdate )

        HandleNextPage qs resp ->
            ( model, Cmd.none, SetPlants resp qs )


view : SharedState -> Model -> Html Msg
view state model =
    let
        qs =
            state.query
    in
    container []
        [ styledH3 [] [ text "plant list" ]
        , renderLegend legendList
        , renderPagingButtons model state.plants
        , renderPlants state.plants
        , queryContainer [] [ text (toQuery qs) ]
        ]


renderPagingButtons : Model -> WebData PlantsResponse -> Html Msg
renderPagingButtons model resp =
    case resp of
        RemoteData.NotAsked ->
            div [] []

        RemoteData.Failure _ ->
            div [] []

        RemoteData.Loading ->
            div [] []

        RemoteData.Success plants ->
            let
                count =
                    plants.count

                limit =
                    20

                totalPages =
                    B.ceiling (B.toFloat count / limit)

                decPage =
                    if model.page == 1 then
                        1

                    else
                        model.page - 1

                incPage =
                    if model.page == totalPages then
                        totalPages

                    else
                        model.page + 1
            in
            pagingContainer []
                [ pagingButton [ onClick (SetPage 1) ] [ text "<<" ]
                , pagingButton [ onClick (SetPage decPage) ] [ text "<" ]
                , pageText [] [ text (String.fromInt model.page) ]
                , pagingButton [ onClick (SetPage incPage) ] [ text ">" ]
                , pagingButton [ onClick (SetPage totalPages) ] [ text ">>" ]
                ]


renderPlants : WebData PlantsResponse -> Html Msg
renderPlants resp =
    case resp of
        RemoteData.NotAsked ->
            div [] [ text "Loading..." ]

        RemoteData.Loading ->
            div [] [ text "loading..." ]

        RemoteData.Success plants ->
            div []
                (List.map renderPlant plants.rows)

        RemoteData.Failure _ ->
            div [] [ text "Loading..." ]


type alias LegendConfig =
    { text : String
    , icon : String
    }


legendList : List LegendConfig
legendList =
    [ { text = "plant_guides", icon = emoji.book }
    , { text = "characteristics", icon = emoji.chart }
    , { text = "fact_sheets", icon = emoji.sheet }
    , { text = "photo_gallery", icon = emoji.image }
    ]


renderLegend : List LegendConfig -> Html Msg
renderLegend configList =
    let
        items =
            List.map (\c -> legendItem [] [ text (c.icon ++ ": " ++ c.text) ]) configList
    in
    legendContainer [] items


hasStrVal : String -> Bool
hasStrVal value =
    not (String.isEmpty value)


getIconList : Plant -> List ( String, String )
getIconList plant =
    [ ( plant.plant_guides, emoji.book )
    , ( plant.characteristics_data, emoji.chart )
    , ( plant.fact_sheets, emoji.sheet )
    , ( plant.image_gallery, emoji.image )
    ]


renderIcons : Plant -> Html Msg
renderIcons plant =
    let
        iconList =
            List.filter (\t -> hasStrVal (Tuple.first t)) (getIconList plant)

        icons =
            List.map (Tuple.second >> (\i -> text (i ++ " "))) iconList
    in
    iconContainer [] icons


renderPlant : Plant -> Html Msg
renderPlant plant =
    let
        common =
            if String.isEmpty plant.common_name then
                span [] []

            else
                span []
                    [ span [] [ text " | " ]
                    , Html.Styled.em [] [ text plant.common_name ]
                    ]

        plantSymbol =
            span []
                [ span [] [ text " | " ]
                , span [] [ text plant.symbol ]
                ]
    in
    plantContainer [ onClick (SelectPlant plant), attribute "role" "button" ]
        [ div []
            [ strong [] [ text plant.scientific_name ]
            , common
            , plantSymbol
            ]
        , renderIcons plant
        ]


container : StyledEl div
container =
    styled div
        [ padding (rem 1)
        , fontSize (rem 1)
        , marginBottom (rem 3)
        ]


plantContainer : StyledEl div
plantContainer =
    styled div
        [ textAlign left
        , padding (rem 0.7)
        , borderBottom3 (px 1) dotted (hex "#000")
        , displayFlex
        , justifyContent spaceBetween
        , cursor pointer
        , hover [ backgroundColor (hex "#eee") ]
        ]


iconContainer : StyledEl div
iconContainer =
    styled div
        [ textAlign right
        , marginLeft (rem 2)
        ]


legendItem : StyledEl span
legendItem =
    styled span
        [ marginBottom (rem 0.5)
        , display inlineBlock
        , marginRight (rem 1)
        ]


legendContainer : StyledEl div
legendContainer =
    styled div
        [ margin2 (rem 1) (rem 0)
        , fontSize (rem 0.8)
        ]


styledH3 : StyledEl h3
styledH3 =
    styled h3
        []


queryContainer : StyledEl div
queryContainer =
    styled div
        [ position fixed
        , bottom (px 0)
        , right (px 0)
        , left (px 0)
        , backgroundColor (hex "#fff")
        , border3 (px 1) dashed (hex "#000")
        , padding (rem 1)
        ]
