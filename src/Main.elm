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
    , fieldCategories : WebData (List FieldCategory)
    }


initModel : () -> ( Model, Cmd Msg )
initModel _ =
    ( { values = Dict.empty
      , fields = []
      , openCategories = []
      , fieldCategories = RemoteData.NotAsked
      }
    , getCategories
    )


view : Model -> Html Msg
view model =
    container []
        [ h2 [] [ text "plants" ]
        , Html.Styled.em [] [ text "select search criteria" ]
        , subContainer []
            [ renderFieldCategories model ]
        , subContainer []
            [ renderInputs model ]
        , subContainer []
            [ styledButton [] [ onClick GetCategories ] [ text "reset" ]
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


renderFields : Model -> Field -> Html Msg
renderFields model field =
    let
        test =
            Debug.log "field" field

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


renderFieldCategories : Model -> Html Msg
renderFieldCategories model =
    case model.fieldCategories of
        RemoteData.NotAsked ->
            div [] [ text "loading..." ]

        RemoteData.Loading ->
            div [] [ text "loading..." ]

        RemoteData.Success cats ->
            let
                children =
                    (List.append [ h3 [] [ text "criteria" ] ] (List.map (renderCategory model) cats))
            in
                div []
                    children

        RemoteData.Failure err ->
            let
                test =
                    Debug.log "err" err
            in
                div [] [ text "something went wrong" ]


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
        categoryContainer []
            [ styledH4 [ onClick (ToggleCategory category.name) ] [ text category.name ]
            , fieldsContainer [] criteria
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

        header =
            if (List.isEmpty fieldList) then
                []
            else
                [ h3 [] [ text "selected criteria" ] ]

        children =
            (List.append header (List.map renderInput fieldList))
    in
        div []
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
    in
        inputContainer []
            [ styledLabel [] [ text (String.append inputType.field.key ": ") ]
            , styledSelect [ value val, onInput (SetValue inputType.field.title) ]
                (List.map renderOption inputType.field.options)
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
        [--  displayFlex
         -- , alignItems flexStart
         -- , flexWrap wrap
         -- , overflowX scroll
         -- , maxWidth (vw 200)
        ]


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
        , marginRight (rem 1)
        , padding (px 4)
        ]


type Msg
    = Reset
    | SetValue String String
    | Submit
    | SelectField Field
    | ToggleCategory String
    | GetCategories
    | SetCategories (WebData (List FieldCategory))


url : String
url =
    "http://localhost:9001/category"


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
