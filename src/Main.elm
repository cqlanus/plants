module Main exposing (main)

import Browser
import Http
import Css exposing (..)
import Dict exposing (Dict)
import VirtualDom
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (value, multiple, size, class, style)
import Html.Styled.Events exposing (onInput, onClick)
import Json.Decode as Decode exposing (Decoder, succeed, string, list)
import Json.Decode.Pipeline as Json exposing (optional, optionalAt, required, requiredAt)
import RemoteData exposing (RemoteData, WebData)


-- [ { name, critiera: [ { key, display, options: [ { value, display } ]} ] } ]


type alias StyledEl msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type alias FieldOption =
    { display : String
    , value : String
    }


type alias Field =
    { title : String
    , options : List FieldOption
    , key : String
    }


type alias FieldCriteria =
    List Field


type alias FieldCategory =
    { name : String
    , criteria : FieldCriteria
    }


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
    [ "genus", "species", "symbol" ]
simpleFields : List Field
simpleFields =
    [ { title= "Genus", key= "genus", options= []  }
    , { title= "Species", key= "species", options= []  }
    , { title= "Symbol", key= "symbol", options= []  }
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
            List.any (\s -> s == section) model.openSections

        header =
            (renderHeader model section)

        fields =
            if showSection then
                (List.map (renderFields model) simpleFields)
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
                    (List.append (renderHeader model section) (List.map (renderCategory model section) cats))
            in
                div []
                    children

        RemoteData.Failure err ->
            let
                test =
                    Debug.log "err" err
            in
                div [] [ text "something went wrong" ]


renderHeader : Model -> String -> List (Html Msg)
renderHeader model headerName =
    let
        showSection =
            List.any (\s -> s == headerName) model.openSections

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
            List.any (\c -> c == category.name) model.openCategories

        showSection =
            List.any (\s -> s == section) model.openSections

        criteria =
            if isOpen then
                (List.map (renderFields model) category.criteria)
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

getTextFieldData : Model -> Field -> { field : Field, value : Maybe String }
getTextFieldData model field =
    let
        val =
            Dict.get field.title model.values
    in
        { field = field, value = val }

getFieldData : Model -> Field -> { field : Field, value : Maybe String }
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
            List.any (\s -> s == section) model.openSections

        inputs =
            if showSection then
                (List.map renderInput fieldList)
            else
                []

        children =
            (List.append (renderHeader model section) inputs)
    in
        selectedCriteriaContainer []
            children


renderInput : { field : Field, value : Maybe String } -> Html Msg
renderInput inputType =
    let
        val =
            case inputType.value of
                Nothing ->
                    ""

                Just v ->
                    v
        isText =
            List.any (\f -> inputType.field.key == f) textFields
        attrs = [ value val, onInput (SetValue inputType.field.title) ]
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


optionsContainer : StyledEl div
optionsContainer =
    styled div
        [ displayFlex
        , justifyContent center
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
            List.any (\t -> (Tuple.first t) == "selected") props

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
    | GetCategories
    | SetCategories (WebData (List FieldCategory))


url : String
url =
    "http://localhost:9002/category"


optionDecoder : Decoder FieldOption
optionDecoder =
    Decode.succeed FieldOption
        |> Json.required "display" string
        |> Json.required "value" string


criteriaDecoder : Decoder Field
criteriaDecoder =
    Decode.succeed Field
        |> Json.required "key" string
        |> Json.required "options" (list optionDecoder)
        |> Json.required "title" string


categoryDecoder : Decoder FieldCategory
categoryDecoder =
    Decode.succeed FieldCategory
        |> Json.required "name" string
        |> Json.required "criteria" (list criteriaDecoder)


getCategories : Cmd Msg
getCategories =
    Http.get
        { url = url
        , expect =
            list categoryDecoder
                |> Http.expectJson (RemoteData.fromResult >> SetCategories)
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
                    List.any (\c -> c == section) model.openSections

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
                    List.any (\c -> c == cat) model.openCategories

                openCategories =
                    if hasCat then
                        List.filter (\c -> c /= cat) model.openCategories
                    else
                        model.openCategories ++ [ cat ]
            in
                ( { model | openCategories = openCategories }, Cmd.none )

        GetCategories ->
            ( { model | fieldCategories = RemoteData.Loading }, getCategories )

        SetCategories response ->
            ( { model | fieldCategories = response }, Cmd.none )

        Reset ->
            ( { model | values = Dict.empty, fields = [] }, Cmd.none )

        Submit ->
            let
                test =
                    Debug.log "values" model.values
            in
                ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = initModel
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
