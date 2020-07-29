module Main exposing (main)

import Browser
import Css exposing (..)
import Dict exposing (Dict)
import VirtualDom
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (value, multiple, size, class)
import Html.Styled.Events exposing (onInput, onClick)


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


type alias FieldCategory =
    { name : String
    , criteria : List Field
    }


type alias Model =
    { values : Dict String String
    , fields : List Field
    , openCategories : List String
    }


initModel : Model
initModel =
    { values = Dict.empty
    , fields = []
    , openCategories = []
    }


view : Model -> Html Msg
view model =
    container []
        [ h2 [] [ text "plants" ]
        , Html.Styled.em [] [ text "select search criteria" ]
        , subContainer []
            [ renderFieldCategories model fieldCategories ]
        , subContainer []
            [ renderInputs model ]
        , subContainer []
            [ styledButton [] [ onClick Reset ] [ text "reset" ]
            , styledButton [] [ onClick Submit ] [ text "submit" ]
            ]
        ]


fieldCategories : List FieldCategory
fieldCategories =
    [ { name = "Taxonomy"
      , criteria =
            [ { key = "genus", title = "genus", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
            , { key = "common name", title = "common name", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
            , { key = "symbol", title = "symbol", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
            , { key = "species", title = "species", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
            , { key = "family", title = "family", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
            ]
      }
    , { name = "Ecology"
      , criteria =
            [ { key = "growth habit", title = "growth habit", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
            , { key = "duration", title = "duration", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
            , { key = "native status", title = "native status", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
            ]
      }
    ]


inputFields : List Field
inputFields =
    [ { key = "genus", title = "genus", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
    , { key = "common name", title = "common name", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
    , { key = "symbol", title = "symbol", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
    , { key = "species", title = "species", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
    , { key = "family", title = "family", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
    , { key = "growth habit", title = "growth habit", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
    , { key = "duration", title = "duration", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
    , { key = "native status", title = "native status", options = [ { display = "A", value = "a" }, { display = "B", value = "b" } ] }
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
            [ text field.title ]


renderFieldCategories : Model -> List FieldCategory -> Html Msg
renderFieldCategories model categories =
    div []
        (List.map (renderCategory model) categories)


renderCategory : Model -> FieldCategory -> Html Msg
renderCategory model category =
    let
        isOpen =
            List.any (\c -> c == category.name) model.openCategories

        criteria =
            if isOpen then
                (List.map (renderFields model) category.criteria)
            else
                []
    in
        div []
            [ styledH4 [ onClick (ToggleCategory category.name) ] [ text category.name ]
            , div [] criteria
            ]


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
        fieldList =
            List.map (getFieldData model) model.fields
    in
        div []
            (List.map renderInput fieldList)


renderInput : { field : Field, value : Maybe String } -> Html Msg
renderInput inputType =
    let
        val =
            case inputType.value of
                Nothing ->
                    ""

                Just v ->
                    v
    in
        div []
            [ label [] [ text inputType.field.title ]
            , styledSelect [ value val, onInput (SetValue inputType.field.title) ]
                (List.map renderOption inputType.field.options)
            ]


renderOption : FieldOption -> Html Msg
renderOption opt =
    option [ value opt.value ] [ text opt.display ]


styledH4 : StyledEl h4
styledH4 =
    styled h4
        [ cursor pointer ]


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
            , fontSize inherit
            , hover
                [ fontWeight bold
                ]
            ]


styledSelect : StyledEl select
styledSelect =
    styled select
        [ border3 (px 1) dashed (hex "#000")
        , padding (px 4)
        ]


styledInput : StyledEl input
styledInput =
    styled input
        [ border3 (px 1) dashed (hex "#000")
        , marginRight (rem 1)
        , padding (px 4)
        ]


type Msg
    = Reset
    | SetValue String String
    | Submit
    | SelectField Field
    | ToggleCategory String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetValue key val ->
            let
                updated =
                    Dict.insert key val model.values
            in
                { model | values = updated }

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
                { model | fields = fields, values = values }

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
                { model | openCategories = openCategories }

        Reset ->
            initModel

        Submit ->
            let
                test =
                    Debug.log "model" model
            in
                model


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initModel
        , view = view >> toUnstyled
        , update = update
        }
