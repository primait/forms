module Prima.Form
    exposing
        ( FormField
        , FormFieldConfig
        , Validation(..)
        , autocompleteConfig
        , checkboxConfig
        , datepickerConfig
        , isValid
        , radioConfig
        , render
        , selectConfig
        , textConfig
        )

{-| Components for building a form.
@docs FormField
@docs FormFieldConfig
@docs Validation
@docs autocompleteConfig
@docs checkboxConfig
@docs datepickerConfig
@docs radioConfig
@docs selectConfig
@docs textConfig
@docs isValid
@docs render
-}

import Date exposing (Date)
import DatePicker exposing (..)
import Html exposing (..)
import Html.Attributes
    exposing
        ( checked
        , class
        , classList
        , disabled
        , for
        , id
        , name
        , selected
        , type_
        , value
        )
import Html.Events
    exposing
        ( onClick
        , onInput
        )
import Regex
import Tuple


{-| Defines a Field component for a generic form.
Opaque implementation.
-}
type FormField model msg
    = FormField (FormFieldConfig model msg)


{-| Defines a configuration for a Field component.
Opaque implementation.
-}
type FormFieldConfig model msg
    = FormFieldTextConfig (TextConfig model msg) (List (Validation model))
    | FormFieldTextareaConfig (TextareaConfig model msg) (List (Validation model))
    | FormFieldRadioConfig (RadioConfig model msg) (List (Validation model))
    | FormFieldCheckboxConfig (CheckboxConfig model msg) (List (Validation model))
    | FormFieldSelectConfig (SelectConfig model msg) (List (Validation model))
    | FormFieldDatepickerConfig (DatepickerConfig model msg) (List (Validation model))
    | FormFieldAutocompleteConfig (AutocompleteConfig model msg) (List (Validation model))


type alias TextConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , customAttributes : List (Attribute msg)
    , reader : model -> Maybe String
    , tagger : Maybe String -> msg
    }


type alias TextareaConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , customAttributes : List (Attribute msg)
    , reader : model -> Maybe String
    , tagger : Maybe String -> msg
    }


type alias RadioConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , customAttributes : List (Attribute msg)
    , reader : model -> Maybe String
    , tagger : Maybe String -> msg
    , options : List ( String, String )
    }


type alias CheckboxConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , customAttributes : List (Attribute msg)
    , reader : model -> Bool
    , tagger : Bool -> msg
    }


type alias SelectConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , isOpen : Bool
    , customAttributes : List (Attribute msg)
    , reader : model -> Maybe String
    , toggleTagger : Bool -> msg
    , optionTagger : Maybe String -> msg
    , options : List ( String, String )
    , showEmptyOption : Bool
    }


type alias DatepickerConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , reader : model -> Maybe Date
    , tagger : DatePicker.Msg -> msg
    , instance : DatePicker
    , settings : DatePicker.Settings
    }


type alias AutocompleteConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , customAttributes : List (Attribute msg)
    , filterReader : model -> Maybe String
    , choiceReader : model -> Maybe String
    , filterTagger : Maybe String -> msg
    , choiceTagger : Maybe String -> msg
    , options : List ( String, String )
    }


{-| Input Text configuration method.
-}
textConfig : String -> String -> Bool -> List (Attribute msg) -> (model -> Maybe String) -> (Maybe String -> msg) -> List (Validation model) -> FormField model msg
textConfig slug label isDisabled customAttributes reader tagger validations =
    FormField <| FormFieldTextConfig (TextConfig slug label isDisabled customAttributes reader tagger) validations


{-| Textarea configuration method.
-}
textareaConfig : String -> String -> Bool -> List (Attribute msg) -> (model -> Maybe String) -> (Maybe String -> msg) -> List (Validation model) -> FormField model msg
textareaConfig slug label isDisabled customAttributes reader tagger validations =
    FormField <| FormFieldTextareaConfig (TextareaConfig slug label isDisabled customAttributes reader tagger) validations


{-| Input Radio configuration method.
-}
radioConfig : String -> String -> Bool -> List (Attribute msg) -> (model -> Maybe String) -> (Maybe String -> msg) -> List ( String, String ) -> List (Validation model) -> FormField model msg
radioConfig slug label isDisabled customAttributes reader tagger options validations =
    FormField <| FormFieldRadioConfig (RadioConfig slug label isDisabled customAttributes reader tagger options) validations


{-| Checkbox configuration method.
-}
checkboxConfig : String -> String -> Bool -> List (Attribute msg) -> (model -> Bool) -> (Bool -> msg) -> List (Validation model) -> FormField model msg
checkboxConfig slug label isDisabled customAttributes reader tagger validations =
    FormField <| FormFieldCheckboxConfig (CheckboxConfig slug label isDisabled customAttributes reader tagger) validations


{-| Select configuration method.
-}
selectConfig : String -> String -> Bool -> Bool -> List (Attribute msg) -> (model -> Maybe String) -> (Bool -> msg) -> (Maybe String -> msg) -> List ( String, String ) -> Bool -> List (Validation model) -> FormField model msg
selectConfig slug label isDisabled isOpen customAttributes reader toggleTagger optionTagger options showEmptyOption validations =
    FormField <| FormFieldSelectConfig (SelectConfig slug label isDisabled isOpen customAttributes reader toggleTagger optionTagger options showEmptyOption) validations


{-| Datepicker configuration method. Uses Bogdanp/elm-datepicker under the hood.
-}
datepickerConfig : String -> String -> Bool -> (model -> Maybe Date) -> (DatePicker.Msg -> msg) -> DatePicker -> DatePicker.Settings -> List (Validation model) -> FormField model msg
datepickerConfig slug label isDisabled reader tagger datepicker settings validations =
    FormField <| FormFieldDatepickerConfig (DatepickerConfig slug label isDisabled reader tagger datepicker settings) validations


{-| Autocomplete configuration method.
-}
autocompleteConfig : String -> String -> Bool -> List (Attribute msg) -> (model -> Maybe String) -> (model -> Maybe String) -> (Maybe String -> msg) -> (Maybe String -> msg) -> List ( String, String ) -> List (Validation model) -> FormField model msg
autocompleteConfig slug label isDisabled customAttributes filterReader choiceReader filterTagger choiceTagger options validations =
    FormField <| FormFieldAutocompleteConfig (AutocompleteConfig slug label isDisabled customAttributes filterReader choiceReader filterTagger choiceTagger options) validations


{-| The only available method to Render a component.
-}
render : model -> FormField model msg -> Html msg
render model (FormField opaqueConfig) =
    case opaqueConfig of
        FormFieldTextConfig config validation ->
            renderInput model config validation

        FormFieldTextareaConfig config validation ->
            renderTextarea model config validation

        FormFieldRadioConfig config validation ->
            renderRadio model config validation

        FormFieldCheckboxConfig config validation ->
            renderCheckbox model config validation

        FormFieldSelectConfig config validation ->
            renderSelect model config validation

        FormFieldDatepickerConfig config validation ->
            renderDatepicker model config validation

        FormFieldAutocompleteConfig config validation ->
            renderAutocomplete model config validation


wrapper : List (Html msg) -> Html msg
wrapper =
    div
        [ class "a-form__field" ]


renderLabel : String -> String -> Html msg
renderLabel slug label =
    Html.label
        [ for slug
        , class "a-form__field__label"
        ]
        [ text label
        ]


renderInput : model -> TextConfig model msg -> List (Validation model) -> Html msg
renderInput model ({ reader, tagger, slug, label, isDisabled, customAttributes } as config) validations =
    let
        valid =
            isValid model (FormFieldTextConfig config validations)

        pristine =
            (not << isValid model) (FormFieldTextConfig config [ NotEmpty ])
    in
    wrapper
        [ renderLabel slug label
        , Html.input
            ([ type_ "text"
             , onInput (tagger << normalizeInput)
             , (value << Maybe.withDefault "" << reader) model
             , id slug
             , name slug
             , disabled isDisabled
             , classList
                [ ( "a-form__field__input", True )
                , ( "is-valid", valid )
                , ( "is-invalid", not valid && not pristine )
                , ( "is-pristine", pristine )
                , ( "is-touched", not pristine )
                ]
             ]
                ++ customAttributes
            )
            []
        ]


renderTextarea : model -> TextareaConfig model msg -> List (Validation model) -> Html msg
renderTextarea model ({ reader, tagger, slug, label, isDisabled, customAttributes } as config) validations =
    let
        valid =
            isValid model (FormFieldTextareaConfig config validations)

        pristine =
            (not << isValid model) (FormFieldTextareaConfig config [ NotEmpty ])
    in
    wrapper
        [ renderLabel slug label
        , Html.textarea
            ([ onInput (tagger << normalizeInput)
             , (value << Maybe.withDefault "" << reader) model
             , id slug
             , name slug
             , disabled isDisabled
             , classList
                [ ( "a-form__field__textarea", True )
                , ( "is-valid", valid )
                , ( "is-invalid", not valid && not pristine )
                , ( "is-pristine", pristine )
                , ( "is-touched", not pristine )
                ]
             ]
                ++ customAttributes
            )
            []
        ]


renderRadio : model -> RadioConfig model msg -> List (Validation model) -> Html msg
renderRadio model ({ slug, label, options } as config) validations =
    (wrapper
        << (::) (renderLabel slug label)
        << List.concat
        << List.map (renderRadioOption model config)
    )
        options


renderRadioOption : model -> RadioConfig model msg -> ( String, String ) -> List (Html msg)
renderRadioOption model { reader, tagger, slug, label, options, isDisabled, customAttributes } ( optionName, optionValue ) =
    let
        optionSlug =
            String.join "_" [ slug, String.toLower optionName ]
    in
    [ Html.input
        ([ type_ "radio"
         , onInput (tagger << normalizeInput)
         , value optionValue
         , id optionSlug
         , name slug
         , (checked << (==) optionValue << Maybe.withDefault "" << reader) model
         , disabled isDisabled
         , classList
            [ ( "a-form__field__radio", True )
            ]
         ]
            ++ customAttributes
        )
        []
    , Html.label
        [ for optionSlug
        , class "a-form__field__radio__label"
        ]
        [ text optionName
        ]
    ]


renderCheckbox : model -> CheckboxConfig model msg -> List (Validation model) -> Html msg
renderCheckbox model { reader, tagger, slug, label, isDisabled, customAttributes } validations =
    wrapper
        [ renderLabel slug label
        , Html.input
            ([ type_ "checkbox"
             , (onClick << tagger << not << reader) model
             , (value << toString << reader) model
             , id slug
             , name slug
             , disabled isDisabled
             , classList
                [ ( "a-form__field__checkbox", True )
                ]
             ]
                ++ customAttributes
            )
            []
        , Html.label
            [ for slug
            , class "a-form__field__checkbox__label"
            ]
            [ text " "
            ]
        ]


renderSelect : model -> SelectConfig model msg -> List (Validation model) -> Html msg
renderSelect model ({ slug, label, reader, optionTagger, showEmptyOption, isDisabled, customAttributes } as config) validations =
    let
        options =
            if showEmptyOption then
                ( "", "" ) :: config.options
            else
                config.options

        valid =
            isValid model (FormFieldSelectConfig config validations)

        pristine =
            (not << isValid model) (FormFieldSelectConfig config [ NotEmpty ])
    in
    wrapper
        [ renderLabel slug label
        , renderCustomSelect model config validations
        , Html.select
            ([ onInput (optionTagger << normalizeInput)
             , id slug
             , name slug
             , disabled isDisabled
             , classList
                [ ( "a-form__field__select", True )
                , ( "is-valid", valid )
                , ( "is-invalid", not valid && not pristine )
                , ( "is-pristine", pristine )
                , ( "is-touched", not pristine )
                ]
             ]
                ++ customAttributes
            )
            (List.map (renderSelectOption model config) options)
        ]


renderSelectOption : model -> SelectConfig model msg -> ( String, String ) -> Html msg
renderSelectOption model { reader, slug, label } ( optionName, optionValue ) =
    option
        [ value optionValue
        , (selected << (==) optionValue << Maybe.withDefault "" << reader) model
        ]
        [ text optionName
        ]


renderCustomSelect : model -> SelectConfig model msg -> List (Validation model) -> Html msg
renderCustomSelect model ({ slug, label, reader, toggleTagger, isDisabled, isOpen, customAttributes } as config) validations =
    let
        options =
            config.options

        valid =
            isValid model (FormFieldSelectConfig config validations)

        pristine =
            (not << isValid model) (FormFieldSelectConfig config [ NotEmpty ])

        currentValue =
            options
                |> List.filter (\( optionName, optionValue ) -> ((==) optionValue << Maybe.withDefault "" << reader) model)
                |> List.map Tuple.first
                |> List.head
                |> Maybe.withDefault ""
    in
    div
        [ classList
            [ ( "a-form__field__customSelect", True )
            , ( "is-open", isOpen )
            , ( "is-valid", valid )
            , ( "is-invalid", not valid && not pristine )
            , ( "is-pristine", pristine )
            , ( "is-touched", not pristine )
            , ( "is-disabled", isDisabled )
            ]
        ]
        [ span
            [ class "a-form__field__customSelect__status"
            , (onClick << toggleTagger << not) isOpen
            ]
            [ text currentValue
            ]
        , ul
            [ class "a-form__field__customSelect__list" ]
            (List.map (renderCustomSelectOption model config) options)
        ]


renderCustomSelectOption : model -> SelectConfig model msg -> ( String, String ) -> Html msg
renderCustomSelectOption model { reader, optionTagger, slug, label } ( optionName, optionValue ) =
    li
        [ classList
            [ ( "a-form__field__customSelect__list__item", True )
            , ( "is-selected", ((==) optionValue << Maybe.withDefault "" << reader) model )
            ]
        , (onClick << optionTagger << normalizeInput) optionValue
        ]
        [ text optionName
        ]


renderDatepicker : model -> DatepickerConfig model msg -> List (Validation model) -> Html msg
renderDatepicker model { reader, tagger, slug, label, instance, settings } validations =
    Html.map tagger (DatePicker.view (reader model) settings instance)


renderAutocomplete : model -> AutocompleteConfig model msg -> List (Validation model) -> Html msg
renderAutocomplete model ({ filterReader, filterTagger, slug, label, isDisabled, customAttributes, options } as config) validations =
    let
        valid =
            isValid model (FormFieldAutocompleteConfig config validations)

        pristine =
            (not << isValid model) (FormFieldAutocompleteConfig config [ NotEmpty ])
    in
    wrapper
        [ renderLabel slug label
        , div
            [ class "a-form__field__autocomplete" ]
            [ Html.input
                ([ type_ "text"
                 , onInput (filterTagger << normalizeInput)
                 , (value << Maybe.withDefault "" << filterReader) model
                 , id slug
                 , name slug
                 , disabled isDisabled
                 , classList
                    [ ( "a-form__field__input", True )
                    , ( "is-valid", valid )
                    , ( "is-invalid", not valid && not pristine )
                    , ( "is-pristine", pristine )
                    , ( "is-touched", not pristine )
                    ]
                 ]
                    ++ customAttributes
                )
                []
            , ul
                [ class "a-form__field__autocomplete__list" ]
                (List.map (renderAutocompleteOption model config) options)
            ]
        ]


renderAutocompleteOption : model -> AutocompleteConfig model msg -> ( String, String ) -> Html msg
renderAutocompleteOption model ({ choiceReader, choiceTagger } as config) ( optionName, optionValue ) =
    li
        [ classList
            [ ( "a-form__field__autocomplete__list__item", True )
            , ( "is-selected", ((==) optionValue << Maybe.withDefault "" << choiceReader) model )
            ]
        , (onClick << choiceTagger << normalizeInput) optionValue
        ]
        [ text optionName
        ]


{-| Validation rules for a FormField.
-}
type Validation model
    = NotEmpty
    | Expression Regex.Regex
    | Custom (model -> Bool)


{-| Validate a FormField.
-}
isValid : model -> FormFieldConfig model msg -> Bool
isValid model opaqueConfig =
    let
        rules =
            case opaqueConfig of
                FormFieldTextConfig _ validations ->
                    validations

                FormFieldTextareaConfig _ validations ->
                    validations

                FormFieldRadioConfig _ validations ->
                    validations

                FormFieldSelectConfig _ validations ->
                    validations

                FormFieldCheckboxConfig _ validations ->
                    validations

                FormFieldDatepickerConfig _ validations ->
                    validations

                FormFieldAutocompleteConfig _ validations ->
                    validations
    in
    List.all (validate model opaqueConfig) rules


validate : model -> FormFieldConfig model msg -> Validation model -> Bool
validate model config validation =
    case ( validation, config ) of
        ( NotEmpty, FormFieldTextConfig { reader } _ ) ->
            (not << isEmpty << Maybe.withDefault "" << reader) model

        ( NotEmpty, FormFieldTextareaConfig { reader } _ ) ->
            (not << isEmpty << Maybe.withDefault "" << reader) model

        ( NotEmpty, FormFieldRadioConfig { reader } _ ) ->
            (not << isEmpty << Maybe.withDefault "" << reader) model

        ( NotEmpty, FormFieldSelectConfig { reader } _ ) ->
            (not << isEmpty << Maybe.withDefault "" << reader) model

        ( Expression exp, FormFieldTextConfig { reader } _ ) ->
            (Regex.contains exp << Maybe.withDefault "" << reader) model

        ( Expression exp, FormFieldTextareaConfig { reader } _ ) ->
            (Regex.contains exp << Maybe.withDefault "" << reader) model

        ( Custom validator, _ ) ->
            validator model

        ( _, _ ) ->
            True


normalizeInput : String -> Maybe String
normalizeInput str =
    if isEmpty str then
        Nothing
    else
        Just str


isEmpty : String -> Bool
isEmpty =
    (==) "" << String.trim
