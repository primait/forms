module Prima.Form
    exposing
        ( AutocompleteOption
        , CheckboxOption
        , FormField
        , FormFieldConfig
        , RadioOption
        , SelectOption
        , Validation(..)
        , autocompleteConfig
        , checkboxConfig
        , checkboxWithOptionsConfig
        , datepickerConfig
        , isValid
        , radioConfig
        , render
        , selectConfig
        , textConfig
        , textareaConfig
        )

{-| Components for building a form.
@docs FormField
@docs FormFieldConfig
@docs Validation
@docs autocompleteConfig
@docs checkboxConfig
@docs checkboxWithOptionsConfig
@docs datepickerConfig
@docs radioConfig
@docs selectConfig
@docs textConfig
@docs textareaConfig
@docs RadioOption
@docs SelectOption
@docs CheckboxOption
@docs AutocompleteOption
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
    | FormFieldCheckboxWithOptionsConfig (CheckboxWithOptionsConfig model msg) (List (Validation model))
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
    , appendableHtml : Maybe (Html msg)
    }


type alias TextareaConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , customAttributes : List (Attribute msg)
    , reader : model -> Maybe String
    , tagger : Maybe String -> msg
    , appendableHtml : Maybe (Html msg)
    }


type alias RadioConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , customAttributes : List (Attribute msg)
    , reader : model -> Maybe String
    , tagger : Maybe String -> msg
    , options : List RadioOption
    , appendableHtml : Maybe (Html msg)
    }


{-| Radio's single option.
-}
type alias RadioOption =
    { label : String
    , slug : String
    }


type alias CheckboxConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , customAttributes : List (Attribute msg)
    , reader : model -> Bool
    , tagger : Bool -> msg
    , appendableHtml : Maybe (Html msg)
    }


type alias CheckboxWithOptionsConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , customAttributes : List (Attribute msg)
    , reader : model -> List ( String, Bool )
    , tagger : String -> Bool -> msg
    , options : List CheckboxOption
    , appendableHtml : Maybe (Html msg)
    }


{-| Checkbox's single option. Can be only used with `checkboxWithOptionsConfig`.
-}
type alias CheckboxOption =
    { label : String
    , slug : String
    , isChecked : Bool
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
    , options : List SelectOption
    , showEmptyOption : Bool
    , appendableHtml : Maybe (Html msg)
    }


{-| Select's single option.
-}
type alias SelectOption =
    { label : String
    , slug : String
    }


type alias DatepickerConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , reader : model -> Maybe Date
    , tagger : DatePicker.Msg -> msg
    , instance : DatePicker
    , settings : DatePicker.Settings
    , appendableHtml : Maybe (Html msg)
    }


type alias AutocompleteConfig model msg =
    { slug : String
    , label : String
    , isDisabled : Bool
    , isOpen : Bool
    , noResults : Maybe String
    , customAttributes : List (Attribute msg)
    , filterReader : model -> Maybe String
    , choiceReader : model -> Maybe String
    , filterTagger : Maybe String -> msg
    , choiceTagger : Maybe String -> msg
    , options : List AutocompleteOption
    , appendableHtml : Maybe (Html msg)
    }


{-| Autocomplete's single option.
-}
type alias AutocompleteOption =
    { label : String
    , slug : String
    }


{-| Input Text configuration method.
-}
textConfig : String -> String -> Bool -> List (Attribute msg) -> (model -> Maybe String) -> (Maybe String -> msg) -> Maybe (Html msg) -> List (Validation model) -> FormField model msg
textConfig slug label isDisabled customAttributes reader tagger appendableHtml validations =
    FormField <| FormFieldTextConfig (TextConfig slug label isDisabled customAttributes reader tagger appendableHtml) validations


{-| Textarea configuration method.
-}
textareaConfig : String -> String -> Bool -> List (Attribute msg) -> (model -> Maybe String) -> (Maybe String -> msg) -> Maybe (Html msg) -> List (Validation model) -> FormField model msg
textareaConfig slug label isDisabled customAttributes reader tagger appendableHtml validations =
    FormField <| FormFieldTextareaConfig (TextareaConfig slug label isDisabled customAttributes reader tagger appendableHtml) validations


{-| Input Radio configuration method.
-}
radioConfig : String -> String -> Bool -> List (Attribute msg) -> (model -> Maybe String) -> (Maybe String -> msg) -> List RadioOption -> Maybe (Html msg) -> List (Validation model) -> FormField model msg
radioConfig slug label isDisabled customAttributes reader tagger options appendableHtml validations =
    FormField <| FormFieldRadioConfig (RadioConfig slug label isDisabled customAttributes reader tagger options appendableHtml) validations


{-| Checkbox configuration method.
-}
checkboxConfig : String -> String -> Bool -> List (Attribute msg) -> (model -> Bool) -> (Bool -> msg) -> Maybe (Html msg) -> List (Validation model) -> FormField model msg
checkboxConfig slug label isDisabled customAttributes reader tagger appendableHtml validations =
    FormField <| FormFieldCheckboxConfig (CheckboxConfig slug label isDisabled customAttributes reader tagger appendableHtml) validations


{-| Checkbox configuration method.
-}
checkboxWithOptionsConfig : String -> String -> Bool -> List (Attribute msg) -> (model -> List ( String, Bool )) -> (String -> Bool -> msg) -> List CheckboxOption -> Maybe (Html msg) -> List (Validation model) -> FormField model msg
checkboxWithOptionsConfig slug label isDisabled customAttributes reader tagger options appendableHtml validations =
    FormField <| FormFieldCheckboxWithOptionsConfig (CheckboxWithOptionsConfig slug label isDisabled customAttributes reader tagger options appendableHtml) validations


{-| Select configuration method.
-}
selectConfig : String -> String -> Bool -> Bool -> List (Attribute msg) -> (model -> Maybe String) -> (Bool -> msg) -> (Maybe String -> msg) -> List SelectOption -> Bool -> Maybe (Html msg) -> List (Validation model) -> FormField model msg
selectConfig slug label isDisabled isOpen customAttributes reader toggleTagger optionTagger options showEmptyOption appendableHtml validations =
    FormField <| FormFieldSelectConfig (SelectConfig slug label isDisabled isOpen customAttributes reader toggleTagger optionTagger options showEmptyOption appendableHtml) validations


{-| Datepicker configuration method. Uses Bogdanp/elm-datepicker under the hood.
-}
datepickerConfig : String -> String -> Bool -> (model -> Maybe Date) -> (DatePicker.Msg -> msg) -> DatePicker -> DatePicker.Settings -> Maybe (Html msg) -> List (Validation model) -> FormField model msg
datepickerConfig slug label isDisabled reader tagger datepicker datepickerSettings appendableHtml validations =
    let
        settings =
            { datepickerSettings
                | isDisabled = always isDisabled
                , inputClassList = ( "is-disabled", isDisabled ) :: datepickerSettings.inputClassList
            }
    in
    FormField <| FormFieldDatepickerConfig (DatepickerConfig slug label isDisabled reader tagger datepicker settings appendableHtml) validations


{-| Autocomplete configuration method.
-}
autocompleteConfig : String -> String -> Bool -> Bool -> Maybe String -> List (Attribute msg) -> (model -> Maybe String) -> (model -> Maybe String) -> (Maybe String -> msg) -> (Maybe String -> msg) -> List AutocompleteOption -> Maybe (Html msg) -> List (Validation model) -> FormField model msg
autocompleteConfig slug label isDisabled isOpen noResults customAttributes filterReader choiceReader filterTagger choiceTagger options appendableHtml validations =
    FormField <| FormFieldAutocompleteConfig (AutocompleteConfig slug label isDisabled isOpen noResults customAttributes filterReader choiceReader filterTagger choiceTagger options appendableHtml) validations


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

        FormFieldCheckboxWithOptionsConfig config validation ->
            renderCheckboxWithOptions model config validation

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


renderError : String -> Html msg
renderError error =
    span
        [ class "a-form__field__error" ]
        [ text error ]


renderInput : model -> TextConfig model msg -> List (Validation model) -> Html msg
renderInput model ({ reader, tagger, slug, label, isDisabled, customAttributes, appendableHtml } as config) validations =
    let
        valid =
            isValid model (FormFieldTextConfig config validations)

        pristine =
            (not << isValid model) (FormFieldTextConfig config [ NotEmpty "" ])
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
                , ( "is-invalid", not valid )
                , ( "is-pristine", pristine )
                , ( "is-touched", not pristine )
                ]
             ]
                ++ customAttributes
            )
            []
        , (renderIf (not valid && not pristine)
            << renderError
            << String.join " "
            << pickError model
            << FormFieldTextConfig config
          )
            validations
        , renderExtraHtml appendableHtml
        ]


renderTextarea : model -> TextareaConfig model msg -> List (Validation model) -> Html msg
renderTextarea model ({ reader, tagger, slug, label, isDisabled, customAttributes, appendableHtml } as config) validations =
    let
        valid =
            isValid model (FormFieldTextareaConfig config validations)

        pristine =
            (not << isValid model) (FormFieldTextareaConfig config [ NotEmpty "" ])
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
                , ( "is-invalid", not valid )
                , ( "is-pristine", pristine )
                , ( "is-touched", not pristine )
                ]
             ]
                ++ customAttributes
            )
            []
        , (renderIf (not valid && not pristine)
            << renderError
            << String.join " "
            << pickError model
            << FormFieldTextareaConfig config
          )
            validations
        , renderExtraHtml appendableHtml
        ]


renderRadio : model -> RadioConfig model msg -> List (Validation model) -> Html msg
renderRadio model ({ slug, label, options, appendableHtml } as config) validations =
    let
        valid =
            isValid model (FormFieldRadioConfig config validations)
    in
    wrapper
        (renderLabel slug label
            :: (List.concat << List.map (renderRadioOption model config)) options
            ++ (List.singleton
                    << renderIf (not valid)
                    << renderError
                    << String.join " "
                    << pickError model
                    << FormFieldRadioConfig config
               )
                validations
            ++ (List.singleton << renderExtraHtml) appendableHtml
        )


renderRadioOption : model -> RadioConfig model msg -> RadioOption -> List (Html msg)
renderRadioOption model { reader, tagger, slug, label, options, isDisabled, customAttributes } option =
    let
        optionSlug =
            String.join "_" [ slug, String.toLower option.label ]
    in
    [ Html.input
        ([ type_ "radio"
         , onInput (tagger << normalizeInput)
         , value option.slug
         , id optionSlug
         , name slug
         , (checked << (==) option.slug << Maybe.withDefault "" << reader) model
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
        [ text option.label
        ]
    ]


renderCheckbox : model -> CheckboxConfig model msg -> List (Validation model) -> Html msg
renderCheckbox model ({ reader, tagger, slug, label, isDisabled, customAttributes, appendableHtml } as config) validations =
    let
        valid =
            isValid model (FormFieldCheckboxConfig config validations)
    in
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
        , (renderIf (not valid)
            << renderError
            << String.join " "
            << pickError model
            << FormFieldCheckboxConfig config
          )
            validations
        , renderExtraHtml appendableHtml
        ]


renderCheckboxWithOptions : model -> CheckboxWithOptionsConfig model msg -> List (Validation model) -> Html msg
renderCheckboxWithOptions model ({ slug, label, options, appendableHtml } as config) validations =
    let
        valid =
            isValid model (FormFieldCheckboxWithOptionsConfig config validations)
    in
    wrapper
        (renderLabel slug label
            :: (List.concat << List.map (renderCheckboxOption model config)) options
            ++ (List.singleton
                    << renderIf (not valid)
                    << renderError
                    << String.join " "
                    << pickError model
                    << FormFieldCheckboxWithOptionsConfig config
               )
                validations
            ++ (List.singleton << renderExtraHtml) appendableHtml
        )


renderCheckboxOption : model -> CheckboxWithOptionsConfig model msg -> CheckboxOption -> List (Html msg)
renderCheckboxOption model ({ reader, tagger, isDisabled, customAttributes } as config) option =
    let
        slug =
            String.join "_" [ config.slug, option.slug ]
    in
    [ Html.input
        ([ type_ "checkbox"
         , (onClick << tagger option.slug << not) option.isChecked
         , value option.slug
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
        [ text option.label
        ]
    ]


renderSelect : model -> SelectConfig model msg -> List (Validation model) -> Html msg
renderSelect model ({ slug, label, reader, optionTagger, showEmptyOption, isDisabled, customAttributes, appendableHtml } as config) validations =
    let
        options =
            if showEmptyOption then
                SelectOption " " "" :: config.options
            else
                config.options

        valid =
            isValid model (FormFieldSelectConfig config validations)

        pristine =
            (not << isValid model) (FormFieldSelectConfig config [ NotEmpty "" ])
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
                , ( "is-invalid", not valid )
                , ( "is-pristine", pristine )
                , ( "is-touched", not pristine )
                ]
             ]
                ++ customAttributes
            )
            (List.map (renderSelectOption model config) options)
        , (renderIf (not valid && not pristine)
            << renderError
            << String.join " "
            << pickError model
            << FormFieldSelectConfig config
          )
            validations
        , renderExtraHtml appendableHtml
        ]


renderSelectOption : model -> SelectConfig model msg -> SelectOption -> Html msg
renderSelectOption model { reader, slug, label } option =
    Html.option
        [ value option.slug
        , (selected << (==) option.slug << Maybe.withDefault "" << reader) model
        ]
        [ text option.label
        ]


renderCustomSelect : model -> SelectConfig model msg -> List (Validation model) -> Html msg
renderCustomSelect model ({ slug, label, reader, toggleTagger, isDisabled, isOpen, customAttributes } as config) validations =
    let
        options =
            config.options

        valid =
            isValid model (FormFieldSelectConfig config validations)

        pristine =
            (not << isValid model) (FormFieldSelectConfig config [ NotEmpty "" ])

        currentValue =
            options
                |> List.filter (\option -> ((==) option.slug << Maybe.withDefault "" << reader) model)
                |> List.map .label
                |> List.head
                |> Maybe.withDefault ""
    in
    div
        [ classList
            [ ( "a-form__field__customSelect", True )
            , ( "is-open", isOpen )
            , ( "is-valid", valid )
            , ( "is-invalid", not valid )
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


renderCustomSelectOption : model -> SelectConfig model msg -> SelectOption -> Html msg
renderCustomSelectOption model { reader, optionTagger, slug, label } option =
    li
        [ classList
            [ ( "a-form__field__customSelect__list__item", True )
            , ( "is-selected", ((==) option.slug << Maybe.withDefault "" << reader) model )
            ]
        , (onClick << optionTagger << normalizeInput) option.slug
        ]
        [ text option.label
        ]


renderDatepicker : model -> DatepickerConfig model msg -> List (Validation model) -> Html msg
renderDatepicker model ({ reader, tagger, slug, label, isDisabled, instance, settings, appendableHtml } as config) validations =
    let
        valid =
            isValid model (FormFieldDatepickerConfig config validations)

        pristine =
            (not << isValid model) (FormFieldDatepickerConfig config [ NotEmpty "" ])

        formatDate : String -> String
        formatDate date =
            -- From dd/mm/yyyy to yyyy-mm-dd which is required by input[type="date"]
            (String.join "-" << List.reverse)
                [ String.left 2 date
                , (String.left 2 << String.dropLeft 3) date
                , (String.left 4 << String.dropLeft 6) date
                ]
    in
    wrapper
        [ renderLabel slug label
        , Html.map tagger (DatePicker.view (reader model) settings instance)
        , Html.input
            [ type_ "date"
            , onInput (tagger << DatePicker.pick << Result.toMaybe << Date.fromString)
            , (value << Maybe.withDefault "" << Maybe.map (formatDate << settings.dateFormatter) << reader) model
            , id slug
            , name slug
            , disabled isDisabled
            , classList
                [ ( "a-form__field__date", True )
                , ( "is-valid", valid )
                , ( "is-invalid", not valid )
                , ( "is-pristine", pristine )
                , ( "is-touched", not pristine )
                ]
            ]
            []
        , (renderIf (not valid && not pristine)
            << renderError
            << String.join " "
            << pickError model
            << FormFieldDatepickerConfig config
          )
            validations
        , renderExtraHtml appendableHtml
        ]


renderAutocomplete : model -> AutocompleteConfig model msg -> List (Validation model) -> Html msg
renderAutocomplete model ({ filterReader, filterTagger, choiceReader, choiceTagger, slug, label, isDisabled, isOpen, noResults, customAttributes, options, appendableHtml } as config) validations =
    let
        valid =
            isValid model (FormFieldAutocompleteConfig config validations)

        pristine =
            (not << isValid model) (FormFieldAutocompleteConfig config [ NotEmpty "" ])

        valueAttr =
            case choiceReader model of
                Just val ->
                    options
                        |> List.filter (\option -> (Maybe.withDefault False << Maybe.map ((==) option.slug) << choiceReader) model)
                        |> List.map .label
                        |> List.head
                        |> Maybe.withDefault ""
                        |> value

                Nothing ->
                    (value << Maybe.withDefault "" << filterReader) model

        clickAttr =
            case choiceReader model of
                Just _ ->
                    [ (onClick << choiceTagger << normalizeInput) "" ]

                Nothing ->
                    []
    in
    wrapper
        [ renderLabel slug label
        , div
            [ classList
                [ ( "a-form__field__autocomplete", True )
                , ( "is-open", isOpen )
                ]
            ]
            [ Html.input
                ([ type_ "text"
                 , onInput (filterTagger << normalizeInput)
                 , valueAttr
                 , id slug
                 , name slug
                 , disabled isDisabled
                 , classList
                    [ ( "a-form__field__input", True )
                    , ( "is-valid", valid )
                    , ( "is-invalid", not valid )
                    , ( "is-pristine", pristine )
                    , ( "is-touched", not pristine )
                    ]
                 ]
                    ++ customAttributes
                    ++ clickAttr
                )
                []
            , ul
                [ class "a-form__field__autocomplete__list" ]
                (if List.length options > 0 then
                    List.map (renderAutocompleteOption model config) options
                 else
                    (List.singleton << renderAutocompleteNoResults model) config
                )
            ]
        , (renderIf (not valid && not pristine)
            << renderError
            << String.join " "
            << pickError model
            << FormFieldAutocompleteConfig config
          )
            validations
        , renderExtraHtml appendableHtml
        ]


renderAutocompleteOption : model -> AutocompleteConfig model msg -> AutocompleteOption -> Html msg
renderAutocompleteOption model ({ choiceReader, choiceTagger } as config) option =
    li
        [ classList
            [ ( "a-form__field__autocomplete__list__item", True )
            , ( "is-selected", ((==) option.slug << Maybe.withDefault "" << choiceReader) model )
            ]
        , (onClick << choiceTagger << normalizeInput) option.slug
        ]
        [ text option.label
        ]


renderAutocompleteNoResults : model -> AutocompleteConfig model msg -> Html msg
renderAutocompleteNoResults model { noResults } =
    li
        [ class "a-form__field__autocomplete__list__noResults"
        ]
        [ (text << Maybe.withDefault "") noResults
        ]


{-| Validation rules for a FormField.
-}
type Validation model
    = NotEmpty String
    | Expression Regex.Regex String
    | Custom (model -> Bool) String


{-| Validate a FormField.
-}
isValid : model -> FormFieldConfig model msg -> Bool
isValid model opaqueConfig =
    List.all (validate model opaqueConfig) (pickValidationRules opaqueConfig)


validate : model -> FormFieldConfig model msg -> Validation model -> Bool
validate model config validation =
    case ( validation, config ) of
        ( NotEmpty _, FormFieldTextConfig { reader } _ ) ->
            (not << isEmpty << Maybe.withDefault "" << reader) model

        ( NotEmpty _, FormFieldTextareaConfig { reader } _ ) ->
            (not << isEmpty << Maybe.withDefault "" << reader) model

        ( NotEmpty _, FormFieldRadioConfig { reader } _ ) ->
            (not << isEmpty << Maybe.withDefault "" << reader) model

        ( NotEmpty _, FormFieldSelectConfig { reader } _ ) ->
            (not << isEmpty << Maybe.withDefault "" << reader) model

        ( NotEmpty _, FormFieldAutocompleteConfig { choiceReader } _ ) ->
            (not << isEmpty << Maybe.withDefault "" << choiceReader) model

        ( Expression exp _, FormFieldTextConfig { reader } _ ) ->
            (Regex.contains exp << Maybe.withDefault "" << reader) model

        ( Expression exp _, FormFieldTextareaConfig { reader } _ ) ->
            (Regex.contains exp << Maybe.withDefault "" << reader) model

        ( Expression exp _, FormFieldAutocompleteConfig { choiceReader } _ ) ->
            (Regex.contains exp << Maybe.withDefault "" << choiceReader) model

        ( Custom validator _, _ ) ->
            validator model

        ( _, _ ) ->
            True


pickValidationRules : FormFieldConfig model msg -> List (Validation model)
pickValidationRules opaqueConfig =
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

        FormFieldCheckboxWithOptionsConfig _ validations ->
            validations

        FormFieldDatepickerConfig _ validations ->
            validations

        FormFieldAutocompleteConfig _ validations ->
            validations


pickError : model -> FormFieldConfig model msg -> List String
pickError model opaqueConfig =
    List.filterMap
        (\rule ->
            if validate model opaqueConfig rule then
                Nothing
            else
                (Just << pickValidationError) rule
        )
        (pickValidationRules opaqueConfig)


pickValidationError : Validation model -> String
pickValidationError rule =
    case rule of
        NotEmpty error ->
            error

        Expression exp error ->
            error

        Custom customRule error ->
            error


renderExtraHtml : Maybe (Html msg) -> Html msg
renderExtraHtml maybeHtml =
    case maybeHtml of
        Just html ->
            html

        Nothing ->
            text ""


normalizeInput : String -> Maybe String
normalizeInput str =
    if isEmpty str then
        Nothing
    else
        Just str


isEmpty : String -> Bool
isEmpty =
    (==) "" << String.trim


renderIf : Bool -> Html msg -> Html msg
renderIf condition html =
    if condition then
        html
    else
        text ""
