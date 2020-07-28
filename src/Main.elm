module Main exposing (main)

import Browser
import Css exposing (..)
import Dict exposing (Dict)
import VirtualDom
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (value, multiple, size, class)
import Html.Styled.Events exposing (onInput, onClick)


type alias StyledEl msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


type alias Model =
    { values : Dict String String
    , fields : List String
    }


initModel : Model
initModel =
    { values = Dict.empty
    , fields = []
    }


view : Model -> Html Msg
view model =
    container []
        [ h2 [] [ text "plants" ]
        , Html.Styled.em [] [ text "select search criteria" ]
        , optionsContainer [ class "test" ]
            [ subContainer []
                (List.map (renderOptions model) options)
            ]
        , subContainer []
            [renderInputs model]
                        , subContainer []
            [ styledButton [] [ onClick Reset ] [ text "reset" ]
            , styledButton [] [ onClick Submit ] [ text "submit" ]
            ]

        ]


type alias Option =
    { title : String }


options : List Option
options =
    [ { title = "genus" }
    , { title = "common name" }
    , { title = "symbol" }
    , { title = "species" }
    , { title = "family" }
    , { title = "growth habit" }
    , { title = "duration" }
    , { title = "native status" }
    ]


renderOptions : Model -> Option -> Html Msg
renderOptions model opt =
    let
        selected = List.any (\f -> f == opt.title) model.fields
        attrs = if selected then [("selected", True)] else []
    in
    styledButton attrs [ onClick (HandleSelect opt.title) ]
        [ text opt.title ]


renderInputs model =
    let
        getFieldData field =
            let
                val =
                    Dict.get field model.values
            in
                { field = field, value = val }

        fieldList =
            List.map getFieldData model.fields
    in
        div []
            (List.map renderInput fieldList)


renderInput : { field : String, value : Maybe String } -> Html Msg
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
            [ label [] [ text inputType.field ]
            , styledInput [ value val, onInput (SetValue inputType.field) ] []
            ]


subContainer : StyledEl div
subContainer =
    styled div
        [ fontSize (px 16)
        , marginTop (rem 1)]


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


styledButton : List (String, Bool) -> StyledEl button
styledButton props =
    let
        selected =
            List.any (\t -> (Tuple.first t) == "selected") props
        bgColor =
            if selected then "#000" else "#fff"
        fontColor =
            if selected then "#fff" else "#000"

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
    | HandleSelect String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetValue key val ->
            let
                updated =
                    Dict.insert key val model.values
            in
                { model | values = updated }

        HandleSelect val ->
            let
                value =
                    Debug.log "val" val

                hasField =
                    List.any (\f -> f == val) model.fields

                fields =
                    if hasField then
                        List.filter (\f -> f /= val) model.fields
                    else
                        model.fields ++ [ val ]

                values =
                    if hasField then
                        Dict.remove val model.values
                    else
                        model.values
            in
                { model | fields = fields, values = values }

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
