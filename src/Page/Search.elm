module Page.Search exposing (Model, Msg, initModel, update, view)

import Css exposing (..)
import Dict exposing (Dict)
import Field exposing (Field, FieldCategory, FieldData, FieldOption, categoryDecoder)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (list)
import Plant exposing (Plant, plantDecoder)
import QS exposing (serialize)
import RemoteData exposing (WebData)


type alias StyledEl msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type alias Model =
    { values : Dict String String
    , fields : List Field
    , openCategories : List String
    , openSections : List String
    , fieldCategories : WebData (List FieldCategory)
    }


initModel : () -> ( Model, Cmd Msg )
initModel _ =
    ( { values = Dict.empty
      , fields = []
      , openCategories = []
      , fieldCategories = RemoteData.NotAsked
      , openSections = [ "selected criteria", "simple criteria" ]
      }
    , getCategories
    )


view : Model -> Html Msg
view model =
    container []
        [ h2 [] [ text "plants" ]
        , Html.Styled.em [] [ text "select search criteria" ]
        , subContainer []
            [ renderTextCategory model ]
        , subContainer []
            [ renderFieldCategories model ]
        , subContainer []
            [ renderInputs model ]
        , subContainer []
            [ styledButton [] [ onClick Reset ] [ text "reset" ]
            , styledButton [] [ onClick Submit ] [ text "submit" ]
            ]
        ]


textFields : List String
textFields =
    [ "genus", "species", "symbol", "common_name" ]


simpleFields : List Field
simpleFields =
    [ { key = "Genus", title = "genus", options = [] }
    , { key = "Species", title = "species", options = [] }
    , { key = "Symbol", title = "symbol", options = [] }
    , { key = "Common Name", title = "common_name", options = [] }
    ]


renderFields : Model -> Field -> Html Msg
renderFields model field =
    let
        selected =
            List.any (\f -> f.title == field.title) model.fields

        attrs =
            if selected then
                [ ( "selected", True ) ]

            else
                []
    in
    styledButton attrs
        [ onClick (SelectField field) ]
        [ text field.key ]


renderTextCategory : Model -> Html Msg
renderTextCategory model =
    let
        section =
            "simple criteria"

        showSection =
            List.member section model.openSections

        header =
            renderHeader model section

        fields =
            if showSection then
                List.map (renderFields model) simpleFields

            else
                []
    in
    div []
        (List.append header fields)


renderFieldCategories : Model -> Html Msg
renderFieldCategories model =
    case model.fieldCategories of
        RemoteData.NotAsked ->
            div [] [ text "loading..." ]

        RemoteData.Loading ->
            div [] [ text "loading..." ]

        RemoteData.Success cats ->
            let
                section =
                    "advanced criteria"

                children =
                    List.append (renderHeader model section) (List.map (renderCategory model section) cats)
            in
            div []
                children

        RemoteData.Failure _ ->
            div [] [ text "something went wrong" ]


renderHeader : Model -> String -> List (Html Msg)
renderHeader model headerName =
    let
        showSection =
            List.member headerName model.openSections

        emoji =
            if showSection then
                "➖"

            else
                "➕"
    in
    [ styledH3 [ onClick (ToggleSection headerName) ] [ text (headerName ++ " " ++ emoji) ] ]


renderCategory : Model -> String -> FieldCategory -> Html Msg
renderCategory model section category =
    let
        isOpen =
            List.member category.name model.openCategories

        showSection =
            List.member section model.openSections

        criteria =
            if isOpen then
                List.map (renderFields model) category.criteria

            else
                []

        cat =
            if showSection then
                categoryContainer []
                    [ styledH4 [ onClick (ToggleCategory category.name) ] [ text category.name ]
                    , fieldsContainer [] criteria
                    ]

            else
                div [] []
    in
    cat


getFieldData : Model -> Field -> FieldData
getFieldData model field =
    let
        val =
            Dict.get field.title model.values
    in
    { field = field, value = val }


renderInputs : Model -> Html Msg
renderInputs model =
    let
        section =
            "selected criteria"

        fieldList =
            List.map (getFieldData model) model.fields

        showSection =
            List.member section model.openSections

        inputs =
            if showSection then
                List.map renderInput fieldList

            else
                []

        children =
            List.append (renderHeader model section) inputs
    in
    selectedCriteriaContainer []
        children


renderInput : FieldData -> Html Msg
renderInput inputType =
    let
        val =
            case inputType.value of
                Nothing ->
                    ""

                Just v ->
                    v

        isText =
            List.member inputType.field.title textFields

        attrs =
            [ value val, onInput (SetValue inputType.field.title) ]

        input =
            if isText then
                styledInput attrs []

            else
                styledSelect attrs (List.map renderOption inputType.field.options)
    in
    inputContainer []
        [ styledLabel [] [ text (String.append inputType.field.key ": ") ]
        , input
        ]


renderOption : FieldOption -> Html Msg
renderOption opt =
    option [ value opt.value ] [ text opt.display ]


inputContainer : StyledEl div
inputContainer =
    styled div
        [ displayFlex
        , alignItems center
        , marginBottom (rem 0.5)
        ]


categoryContainer : StyledEl div
categoryContainer =
    styled div
        []


fieldsContainer : StyledEl div
fieldsContainer =
    styled div
        []


styledH3 : StyledEl h3
styledH3 =
    styled h3
        [ cursor pointer
        , textDecoration underline
        ]


styledH4 : StyledEl h4
styledH4 =
    styled h4
        [ cursor pointer ]


selectedCriteriaContainer : StyledEl div
selectedCriteriaContainer =
    styled div
        [ marginTop (rem 3)
        ]


subContainer : StyledEl div
subContainer =
    styled div
        [ fontSize (px 16)
        , marginTop (rem 1)
        ]


container : StyledEl div
container =
    styled div
        [ padding (rem 1)
        , fontFamily monospace
        ]


styledButton : List ( String, Bool ) -> StyledEl button
styledButton props =
    let
        selected =
            List.any (\t -> Tuple.first t == "selected") props

        bgColor =
            if selected then
                "#000"

            else
                "#fff"

        fontColor =
            if selected then
                "#fff"

            else
                "#000"
    in
    styled button
        [ backgroundColor (hex bgColor)
        , color (hex fontColor)
        , border3 (px 1) dashed (hex "#000")
        , padding2 (rem 0.5) (rem 1)
        , marginRight (rem 0.5)
        , marginBottom (rem 0.5)
        , cursor pointer
        , fontFamily inherit
        , fontSize (rem 0.7)
        , hover
            [ fontWeight bold
            ]
        ]


styledLabel : StyledEl label
styledLabel =
    styled label
        [ marginRight (rem 1) ]


styledSelect : StyledEl select
styledSelect =
    styled select
        [ border3 (px 1) dashed (hex "#000")
        , padding (rem 0.4)
        , flex (num 1)
        ]


styledInput : StyledEl input
styledInput =
    styled input
        [ border3 (px 1) dashed (hex "#000")
        , padding (rem 0.4)
        , flex (num 1)
        ]


type Msg
    = Reset
    | SetValue String String
    | Submit
    | SelectField Field
    | ToggleCategory String
    | ToggleSection String
    | SetCategories (WebData (List FieldCategory))
    | SetPlants (WebData (List Plant))


base : String
base =
    "http://localhost:9001"


getCategories : Cmd Msg
getCategories =
    Http.get
        { url = base ++ "/category"
        , expect =
            list categoryDecoder
                |> Http.expectJson (RemoteData.fromResult >> SetCategories)
        }


queryReducer : String -> String -> QS.Query -> QS.Query
queryReducer key value query =
    QS.setStr key value query


createQueryString : Dict String String -> QS.Query
createQueryString values =
    let
        query =
            QS.empty

        reduced =
            Dict.foldl queryReducer query values
    in
    reduced


getPlants : Model -> Cmd Msg
getPlants model =
    let
        mapped =
            createQueryString model.values

        qs =
            QS.serialize
                QS.config
                mapped
    in
    Http.get
        { url = base ++ "/plant" ++ qs
        , expect =
            list plantDecoder
                |> Http.expectJson (RemoteData.fromResult >> SetPlants)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetValue key val ->
            let
                updated =
                    Dict.insert key val model.values
            in
            ( { model | values = updated }, Cmd.none )

        SelectField val ->
            let
                hasField =
                    List.any (\f -> f.title == val.title) model.fields

                fields =
                    if hasField then
                        List.filter (\f -> f.title /= val.title) model.fields

                    else
                        model.fields ++ [ val ]

                values =
                    if hasField then
                        Dict.remove val.title model.values

                    else
                        model.values
            in
            ( { model | fields = fields, values = values }, Cmd.none )

        ToggleSection section ->
            let
                hasSection =
                    List.member section model.openSections

                openSections =
                    if hasSection then
                        List.filter (\c -> c /= section) model.openSections

                    else
                        model.openSections ++ [ section ]
            in
            ( { model | openSections = openSections }, Cmd.none )

        ToggleCategory cat ->
            let
                hasCat =
                    List.member cat model.openCategories

                openCategories =
                    if hasCat then
                        List.filter (\c -> c /= cat) model.openCategories

                    else
                        model.openCategories ++ [ cat ]
            in
            ( { model | openCategories = openCategories }, Cmd.none )

        SetCategories response ->
            ( { model | fieldCategories = response }, Cmd.none )

        SetPlants plants ->
            let
                plants_ =
                    Debug.log "plants" plants
            in
            ( model, Cmd.none )

        Reset ->
            ( { model | values = Dict.empty, fields = [] }, Cmd.none )

        Submit ->
            ( model, getPlants model )
