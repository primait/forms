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
        , passwordConfig
        , radioConfig
        , render
        , selectConfig
        , textConfig
        , textareaConfig
        , wrapper
        )

{-| Package to build a Form using [Prima Assicurazioni](https://www.prima.it)'s Design System.

In order to keep the configuration as simple as possible we decided to not allow
CSS classes to be changed, also forcing consistency in our ecosystem.


# Definition

@docs FormField, FormFieldConfig, Validation


# Basic components configuration

@docs textConfig, passwordConfig, textareaConfig, checkboxConfig, CheckboxOption, checkboxWithOptionsConfig, SelectOption, selectConfig, RadioOption, radioConfig


# Custom components configuration

@docs AutocompleteOption
@docs autocompleteConfig

@docs datepickerConfig


# Render a FormField

@docs render, wrapper


# Validate a FormField

@docs isValid

-}

import Date exposing (Date, Day(..), Month(..))
import Date.Format as DateFormat
import DatePicker
import Html exposing (..)
import Html.Attributes
    exposing
        ( attribute
        , checked
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
        ( onBlur
        , onClick
        , onFocus
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
    | FormFieldPasswordConfig (PasswordConfig model msg) (List (Validation model))
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
    , attrs : List (Attribute msg)
    , reader : model -> Maybe String
    , tagger : Maybe String -> msg
    , onFocus : msg
    , onBlur : msg
    }


type alias PasswordConfig model msg =
    { slug : String
    , label : String
    , attrs : List (Attribute msg)
    , reader : model -> Maybe String
    , tagger : Maybe String -> msg
    , onFocus : msg
    , onBlur : msg
    }


type alias TextareaConfig model msg =
    { slug : String
    , label : String
    , attrs : List (Attribute msg)
    , reader : model -> Maybe String
    , tagger : Maybe String -> msg
    , onFocus : msg
    , onBlur : msg
    }


type alias RadioConfig model msg =
    { slug : String
    , label : String
    , attrs : List (Attribute msg)
    , reader : model -> Maybe String
    , tagger : Maybe String -> msg
    , onFocus : msg
    , onBlur : msg
    , options : List RadioOption
    }


{-| Describes an option for a Radio

    [ RadioOption "Italy" "ita" True
    , RadioOption "France" "fra" False
    , RadioOption "Spain" "spa" True
    ]

-}
type alias RadioOption =
    { label : String
    , slug : String
    }


type alias CheckboxConfig model msg =
    { slug : String
    , label : String
    , attrs : List (Attribute msg)
    , reader : model -> Bool
    , tagger : Bool -> msg
    , onFocus : msg
    , onBlur : msg
    }


type alias CheckboxWithOptionsConfig model msg =
    { slug : String
    , label : String
    , attrs : List (Attribute msg)
    , reader : model -> List ( String, Bool )
    , tagger : String -> Bool -> msg
    , onFocus : msg
    , onBlur : msg
    , options : List CheckboxOption
    }


{-| Describes an option for a Checkbox

    [ CheckboxOption "Italy" "ita" True
    , CheckboxOption "France" "fra" False
    , CheckboxOption "Spain" "spa" True
    ]

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
    , placeholder : Maybe String
    , attrs : List (Attribute msg)
    , reader : model -> Maybe String
    , toggleTagger : Bool -> msg
    , optionTagger : Maybe String -> msg
    , onFocus : msg
    , onBlur : msg
    , options : List SelectOption
    }


{-| Describes an option for a Select

    [ SelectOption "Italy" "ita"
    , SelectOption "France" "fra"
    ]

-}
type alias SelectOption =
    { label : String
    , slug : String
    }


type alias DatepickerConfig model msg =
    { slug : String
    , label : String
    , reader : model -> Maybe String
    , tagger : Maybe String -> msg
    , datePickerTagger : DatePicker.Msg -> msg
    , onFocus : msg
    , onBlur : msg
    , instance : DatePicker.Model
    , showDatePicker : Bool
    }


type alias AutocompleteConfig model msg =
    { slug : String
    , label : String
    , isOpen : Bool
    , noResults : Maybe String
    , attrs : List (Attribute msg)
    , filterReader : model -> Maybe String
    , choiceReader : model -> Maybe String
    , filterTagger : Maybe String -> msg
    , choiceTagger : Maybe String -> msg
    , onFocus : msg
    , onBlur : msg
    , options : List AutocompleteOption
    }


{-| Describes an option for an Autocomplete

    [ AutocompleteOption "Italy" "ita"
    , AutocompleteOption "France" "fra"
    ]

-}
type alias AutocompleteOption =
    { label : String
    , slug : String
    }


{-| Input Text configuration method.

    import Prima.Form as Form exposing (FormField, FormFieldConfig, Validation(..))
    ...

    type Msg
        = OnInputUsername (Maybe String)
        | OnFocusUsername
        | OnBlurUsername
        ...

    type alias Model =
        { username : Maybe String
        ...
        }

    usernameConfig : FormField Model Msg
    usernameConfig  =
        textConfig
            "username"
            "Username:"
            [ minlength 3, maxlength 12, disabled False ]
            .username
            OnInputUsername
            OnFocusUsername
            OnBlurUsername
            [ NotEmpty "Empty value is not acceptable."
            , Custom ((<=) 3 << String.length << Maybe.withDefault "" << .username) "Value must be between 3 and 12 characters length."
            ]

-}
textConfig : String -> String -> List (Attribute msg) -> (model -> Maybe String) -> (Maybe String -> msg) -> msg -> msg -> List (Validation model) -> FormField model msg
textConfig slug label attrs reader tagger onFocus onBlur validations =
    FormField <| FormFieldTextConfig (TextConfig slug label attrs reader tagger onFocus onBlur) validations


{-| Input password configuration method. See `textConfig` for configuration options.
-}
passwordConfig : String -> String -> List (Attribute msg) -> (model -> Maybe String) -> (Maybe String -> msg) -> msg -> msg -> List (Validation model) -> FormField model msg
passwordConfig slug label attrs reader tagger onFocus onBlur validations =
    FormField <| FormFieldPasswordConfig (PasswordConfig slug label attrs reader tagger onFocus onBlur) validations


{-| Textarea configuration method. See `textConfig` for configuration options.
-}
textareaConfig : String -> String -> List (Attribute msg) -> (model -> Maybe String) -> (Maybe String -> msg) -> msg -> msg -> List (Validation model) -> FormField model msg
textareaConfig slug label attrs reader tagger onFocus onBlur validations =
    FormField <| FormFieldTextareaConfig (TextareaConfig slug label attrs reader tagger onFocus onBlur) validations


{-| Input Radio configuration method.

    import Prima.Form as Form exposing (FormField, FormFieldConfig, Validation(..))
    ...

    type Msg
        = OnChangeGender (Maybe String)
        | OnFocusGender
        | OnBlurGender
        ...

    type alias Model =
        { gender : Maybe String
        ...
        }

    genderConfig : FormField Model Msg
    genderConfig =
        Form.radioConfig
            "gender"
            "Gender"
            []
            .gender
            OnChangeGender
            OnFocusGender
            OnBlurGender
            [ RadioOption "Male" "male" , RadioOption "Female" "female" ]
            [ Custom ((==) "female" << Maybe.withDefault "female" << .gender) "You must select `Female` to proceed." ]

-}
radioConfig : String -> String -> List (Attribute msg) -> (model -> Maybe String) -> (Maybe String -> msg) -> msg -> msg -> List RadioOption -> List (Validation model) -> FormField model msg
radioConfig slug label attrs reader tagger onFocus onBlur options validations =
    FormField <| FormFieldRadioConfig (RadioConfig slug label attrs reader tagger onFocus onBlur options) validations


{-| Checkbox with single option configuration method.

    import Prima.Form as Form exposing (FormField, FormFieldConfig, Validation(..))
    ...

    type Msg
        = OnChangePrivacy Bool
        | OnFocusPrivacy
        | OnBlurPrivacy
        ...

    type alias Model =
        { privacy : Bool
        ...
        }

    ...
    acceptPrivacyConfig : FormField Model Msg
    acceptPrivacyConfig =
        Form.checkboxConfig
            "privacy"
            "Do you accept our Privacy Policy?"
            []
            .privacy
            OnChangePrivacy
            OnFocusPrivacy
            OnBlurPrivacy
            []

-}
checkboxConfig : String -> String -> List (Attribute msg) -> (model -> Bool) -> (Bool -> msg) -> msg -> msg -> List (Validation model) -> FormField model msg
checkboxConfig slug label attrs reader tagger onFocus onBlur validations =
    FormField <| FormFieldCheckboxConfig (CheckboxConfig slug label attrs reader tagger onFocus onBlur) validations


{-| Checkbox with multiple option configuration method.

    import Prima.Form as Form exposing (FormField, FormFieldConfig, Validation(..))
    ...

    type Msg
        = OnChangeVisitedCountries String Bool
        | OnFocusVisitedCountries
        | OnBlurVisitedCountries
        ...

    type alias Model =
        { visitedCountries : List (String, String, Bool)
        ...
        }

    ...
    visitedCountriesConfig : List ( String, String, Bool ) -> FormField Model Msg
    visitedCountriesConfig options =
        Form.checkboxWithOptionsConfig
            "visited_countries"
            "Visited countries"
            []
            (List.map (\( label, slug, checked ) -> ( slug, checked )) << .visitedCountries)
            OnChangeVisitedCountries
            OnFocusVisitedCountries
            OnBlurVisitedCountries
            (List.map (\( label, slug, checked ) -> CheckboxOption label slug checked) options)
            []

-}
checkboxWithOptionsConfig : String -> String -> List (Attribute msg) -> (model -> List ( String, Bool )) -> (String -> Bool -> msg) -> msg -> msg -> List CheckboxOption -> List (Validation model) -> FormField model msg
checkboxWithOptionsConfig slug label attrs reader tagger onFocus onBlur options validations =
    FormField <| FormFieldCheckboxWithOptionsConfig (CheckboxWithOptionsConfig slug label attrs reader tagger onFocus onBlur options) validations


{-| Select configuration method.

    import Prima.Form as Form exposing (FormField, FormFieldConfig, Validation(..))
    ...

    type Msg
        = OnChangeCity (Maybe String)
        | OnFocusCity
        | OnBlurCity
        | ToggleCity
        ...

    type alias Model =
        { city : Maybe String
        , isOpenCitySelect : Bool
        ...
        }

    ...

    cityConfig : Bool -> FormField Model Msg
    cityConfig isOpenCitySelect =
        Form.selectConfig
            "city"
            "City"
            False
            isOpenCitySelect
            (Just "Select any option")
            []
            .city
            ToggleCity
            OnChangeCity
            OnFocusCity
            OnBlurCity
            (List.sortBy .label [ SelectOption "Milan" "MI" , SelectOption "Turin" "TO" , SelectOption "Rome" "RO" , SelectOption "Naples" "NA" , SelectOption "Genoa" "GE" ] )
            [ NotEmpty "Empty value is not acceptable." ]

-}
selectConfig : String -> String -> Bool -> Bool -> Maybe String -> List (Attribute msg) -> (model -> Maybe String) -> (Bool -> msg) -> (Maybe String -> msg) -> msg -> msg -> List SelectOption -> List (Validation model) -> FormField model msg
selectConfig slug label isDisabled isOpen placeholder attrs reader toggleTagger optionTagger onFocus onBlur options validations =
    FormField <| FormFieldSelectConfig (SelectConfig slug label isDisabled isOpen placeholder attrs reader toggleTagger optionTagger onFocus onBlur options) validations


{-| Datepicker configuration method. Uses [Leonti/elm-material-datepicker](http://package.elm-lang.org/packages/Leonti/elm-material-datepicker/latest) under the hood.

    import DatePicker
    import Date.Format
    import Prima.Form as Form exposing (FormField, FormFieldConfig, Validation(..))
    ...

    type Msg
        = OnInputBirthDate (Maybe String)
        | OnFocusBirthDate
        | OnBlurBirthDate
        | OnChangeBirthDateDatePicker DatePicker.Msg
        ...

    type alias Model =
      { dateOfBirth : Maybe String
      , dateOfBirthDP: DatePicker.Model
      ...
      }

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
          OnChangeBirthDateDatePicker dpMsg ->
              let
                  updatedInstance =
                      DatePicker.update dpMsg model.dateOfBirthDP
              in
              { model | dateOfBirthDP = updatedInstance, dateOfBirth = (Just << Date.Format.format "%d/%m/%Y" << DatePicker.selectedDate) updatedInstance } ! []
          ...

-}
datepickerConfig : String -> String -> (model -> Maybe String) -> (Maybe String -> msg) -> (DatePicker.Msg -> msg) -> msg -> msg -> DatePicker.Model -> Bool -> List (Validation model) -> FormField model msg
datepickerConfig slug label reader tagger datePickerTagger onFocus onBlur datepicker showDatePicker validations =
    FormField <| FormFieldDatepickerConfig (DatepickerConfig slug label reader tagger datePickerTagger onFocus onBlur datepicker showDatePicker) validations


{-| Autocomplete configuration method.

    import Prima.Form as Form exposing (FormField, FormFieldConfig, Validation(..))
    ...

    type Msg
        = OnSelectCountry (Maybe String)
        | OnFilterCountry (Maybe String)
        | OnFocusCountry
        | OnBlurCountry
        ...

    type alias Model =
        { country : Maybe String
        , countryFilter : Maybe String
        , isOpenCountryAutocomplete: Bool
        ...
        }

    ...

    countryConfig : Bool -> Maybe String -> FormField Model Msg
    countryConfig isOpenCountryAutocomplete countryFilter =
        let
            lowerFilter =
                (String.toLower << Maybe.withDefault "") countryFilter
        in
        Form.autocompleteConfig
            "country"
            "Country"
            isOpenCountry
            (Just "No results")
            []
            .countryFilter
            .country
            OnFilterCountry
            OnSelectCountry
            OnFocusCountry
            OnBlurCountry
            (List.filter (String.contains lowerFilter << String.toLower << .label) <| [ AutocompleteOption "Italy" "ITA", AutocompleteOption "Brasil" "BRA", AutocompleteOption "France" "FRA", AutocompleteOption "England" "ENG", AutocompleteOption "USA" "USA", AutocompleteOption "Japan" "JAP" ])
            [ NotEmpty "Empty value is not acceptable." ]

-}
autocompleteConfig : String -> String -> Bool -> Maybe String -> List (Attribute msg) -> (model -> Maybe String) -> (model -> Maybe String) -> (Maybe String -> msg) -> (Maybe String -> msg) -> msg -> msg -> List AutocompleteOption -> List (Validation model) -> FormField model msg
autocompleteConfig slug label isOpen noResults attrs filterReader choiceReader filterTagger choiceTagger onFocus onBlur options validations =
    FormField <| FormFieldAutocompleteConfig (AutocompleteConfig slug label isOpen noResults attrs filterReader choiceReader filterTagger choiceTagger onFocus onBlur options) validations


{-| Method for rendering a `FormField`
-}
render : model -> FormField model msg -> List (Html msg)
render model (FormField opaqueConfig) =
    case opaqueConfig of
        FormFieldTextConfig config validation ->
            renderInput model config validation

        FormFieldPasswordConfig config validation ->
            renderPassword model config validation

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


{-| Wrapper for a FormField rendered with `render` function.
-}
wrapper : List (Html msg) -> Html msg
wrapper content =
    div
        [ class "a-form__field"
        ]
        content


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


renderInput : model -> TextConfig model msg -> List (Validation model) -> List (Html msg)
renderInput model ({ reader, tagger, slug, label, attrs } as config) validations =
    let
        valid =
            validate model (FormFieldTextConfig config validations)

        pristine =
            (not << validate model) (FormFieldTextConfig config [ NotEmpty "" ])
    in
    [ renderLabel slug label
    , Html.input
        ([ type_ "text"
         , onInput (tagger << normalizeInput)
         , onFocus config.onFocus
         , onBlur config.onBlur
         , (value << Maybe.withDefault "" << reader) model
         , id slug
         , name slug
         , classList
            [ ( "a-form__field__input", True )
            , ( "is-valid", valid )
            , ( "is-invalid", not valid )
            , ( "is-pristine", pristine )
            , ( "is-touched", not pristine )
            ]
         ]
            ++ attrs
        )
        []
    , (renderIf (not valid && not pristine)
        << renderError
        << String.join " "
        << pickError model
        << FormFieldTextConfig config
      )
        validations
    ]


renderPassword : model -> PasswordConfig model msg -> List (Validation model) -> List (Html msg)
renderPassword model ({ reader, tagger, slug, label, attrs } as config) validations =
    let
        valid =
            validate model (FormFieldPasswordConfig config validations)

        pristine =
            (not << validate model) (FormFieldPasswordConfig config [ NotEmpty "" ])
    in
    [ renderLabel slug label
    , Html.input
        ([ type_ "password"
         , onInput (tagger << normalizeInput)
         , onFocus config.onFocus
         , onBlur config.onBlur
         , (value << Maybe.withDefault "" << reader) model
         , id slug
         , name slug
         , classList
            [ ( "a-form__field__input", True )
            , ( "is-valid", valid )
            , ( "is-invalid", not valid )
            , ( "is-pristine", pristine )
            , ( "is-touched", not pristine )
            ]
         ]
            ++ attrs
        )
        []
    , (renderIf (not valid && not pristine)
        << renderError
        << String.join " "
        << pickError model
        << FormFieldPasswordConfig config
      )
        validations
    ]


renderTextarea : model -> TextareaConfig model msg -> List (Validation model) -> List (Html msg)
renderTextarea model ({ reader, tagger, slug, label, attrs } as config) validations =
    let
        valid =
            validate model (FormFieldTextareaConfig config validations)

        pristine =
            (not << validate model) (FormFieldTextareaConfig config [ NotEmpty "" ])
    in
    [ renderLabel slug label
    , Html.textarea
        ([ onInput (tagger << normalizeInput)
         , onFocus config.onFocus
         , onBlur config.onBlur
         , (value << Maybe.withDefault "" << reader) model
         , id slug
         , name slug
         , classList
            [ ( "a-form__field__textarea", True )
            , ( "is-valid", valid )
            , ( "is-invalid", not valid )
            , ( "is-pristine", pristine )
            , ( "is-touched", not pristine )
            ]
         ]
            ++ attrs
        )
        []
    , (renderIf (not valid && not pristine)
        << renderError
        << String.join " "
        << pickError model
        << FormFieldTextareaConfig config
      )
        validations
    ]


renderRadio : model -> RadioConfig model msg -> List (Validation model) -> List (Html msg)
renderRadio model ({ slug, label, options } as config) validations =
    let
        valid =
            validate model (FormFieldRadioConfig config validations)
    in
    renderLabel slug label
        :: (List.concat << List.map (renderRadioOption model config)) options
        ++ (List.singleton
                << renderIf (not valid)
                << renderError
                << String.join " "
                << pickError model
                << FormFieldRadioConfig config
           )
            validations


renderRadioOption : model -> RadioConfig model msg -> RadioOption -> List (Html msg)
renderRadioOption model ({ reader, tagger, slug, label, options, attrs } as config) option =
    let
        optionSlug =
            (String.join "_" << List.map (String.trim << String.toLower)) [ slug, option.slug ]
    in
    [ Html.input
        ([ type_ "radio"

         {--IE 11 does not behave correctly with onInput --}
         , (onClick << tagger << normalizeInput << .slug) option
         , onFocus config.onFocus
         , onBlur config.onBlur
         , value option.slug
         , id optionSlug
         , name slug
         , (checked << (==) option.slug << Maybe.withDefault "" << reader) model
         , classList
            [ ( "a-form__field__radio", True )
            ]
         ]
            ++ attrs
        )
        []
    , Html.label
        [ for optionSlug
        , class "a-form__field__radio__label"
        ]
        [ text option.label
        ]
    ]


renderCheckbox : model -> CheckboxConfig model msg -> List (Validation model) -> List (Html msg)
renderCheckbox model ({ reader, tagger, slug, label, attrs } as config) validations =
    let
        valid =
            validate model (FormFieldCheckboxConfig config validations)
    in
    [ renderLabel slug label
    , Html.input
        ([ type_ "checkbox"
         , (onClick << tagger << not << reader) model
         , onFocus config.onFocus
         , onBlur config.onBlur
         , (value << toString << reader) model
         , id slug
         , name slug
         , classList
            [ ( "a-form__field__checkbox", True )
            ]
         ]
            ++ attrs
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
    ]


renderCheckboxWithOptions : model -> CheckboxWithOptionsConfig model msg -> List (Validation model) -> List (Html msg)
renderCheckboxWithOptions model ({ slug, label, options } as config) validations =
    let
        valid =
            validate model (FormFieldCheckboxWithOptionsConfig config validations)
    in
    renderLabel slug label
        :: (List.concat << List.map (renderCheckboxOption model config)) options
        ++ (List.singleton
                << renderIf (not valid)
                << renderError
                << String.join " "
                << pickError model
                << FormFieldCheckboxWithOptionsConfig config
           )
            validations


renderCheckboxOption : model -> CheckboxWithOptionsConfig model msg -> CheckboxOption -> List (Html msg)
renderCheckboxOption model ({ reader, tagger, attrs } as config) option =
    let
        slug =
            (String.join "_" << List.map (String.trim << String.toLower)) [ config.slug, option.slug ]
    in
    [ Html.input
        ([ type_ "checkbox"
         , (onClick << tagger option.slug << not) option.isChecked
         , onFocus config.onFocus
         , onBlur config.onBlur
         , value option.slug
         , id slug
         , name slug
         , classList
            [ ( "a-form__field__checkbox", True )
            ]
         ]
            ++ attrs
        )
        []
    , Html.label
        [ for slug
        , class "a-form__field__checkbox__label"
        ]
        [ text option.label
        ]
    ]


renderSelect : model -> SelectConfig model msg -> List (Validation model) -> List (Html msg)
renderSelect model ({ slug, label, reader, optionTagger, attrs } as config) validations =
    let
        options =
            case ( config.placeholder, config.isOpen ) of
                ( Just placeholder, False ) ->
                    SelectOption placeholder "" :: config.options

                ( _, _ ) ->
                    config.options

        valid =
            validate model (FormFieldSelectConfig config validations)

        pristine =
            (not << validate model) (FormFieldSelectConfig config [ NotEmpty "" ])
    in
    [ renderLabel slug label
    , renderCustomSelect model config validations
    , Html.select
        ([ onInput (optionTagger << normalizeInput)
         , onFocus config.onFocus
         , onBlur config.onBlur
         , id slug
         , name slug
         , classList
            [ ( "a-form__field__select", True )
            , ( "is-valid", valid )
            , ( "is-invalid", not valid )
            , ( "is-pristine", pristine )
            , ( "is-touched", not pristine )
            ]
         ]
            ++ attrs
        )
        (List.map (renderSelectOption model config) options)
    , (renderIf (not valid && not pristine)
        << renderError
        << String.join " "
        << pickError model
        << FormFieldSelectConfig config
      )
        validations
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
renderCustomSelect model ({ slug, label, reader, toggleTagger, isDisabled, isOpen, attrs } as config) validations =
    let
        options =
            case ( config.placeholder, isOpen ) of
                ( Just placeholder, False ) ->
                    SelectOption placeholder "" :: config.options

                ( _, _ ) ->
                    config.options

        valid =
            validate model (FormFieldSelectConfig config validations)

        pristine =
            (not << validate model) (FormFieldSelectConfig config [ NotEmpty "" ])

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
        , onFocus config.onFocus
        , onBlur config.onBlur
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


renderDatepicker : model -> DatepickerConfig model msg -> List (Validation model) -> List (Html msg)
renderDatepicker model ({ reader, tagger, datePickerTagger, slug, label, instance, showDatePicker } as config) validations =
    let
        valid =
            validate model (FormFieldDatepickerConfig config validations)

        pristine =
            (not << validate model) (FormFieldDatepickerConfig config [ NotEmpty "" ])

        inputTextFormat str =
            (String.join "/" << List.reverse << String.split "-") str

        inputDateFormat str =
            (String.join "-" << List.reverse << String.split "/") str
    in
    [ renderLabel slug label
    , Html.input
        [ type_ "text"
        , onInput (tagger << normalizeInput)
        , (value << Maybe.withDefault "" << Maybe.map inputTextFormat << reader) model
        , onFocus config.onFocus
        , onBlur config.onBlur
        , id slug
        , name slug
        , classList
            [ ( "a-form__field__input", True )
            , ( "is-valid", valid )
            , ( "is-invalid", not valid )
            , ( "is-pristine", pristine )
            , ( "is-touched", not pristine )
            ]
        ]
        []
    , (renderIf showDatePicker << Html.map datePickerTagger << DatePicker.view) instance
    , Html.input
        [ attribute "type" "date"
        , onInput (tagger << normalizeInput)
        , (value << Maybe.withDefault "" << Maybe.map inputDateFormat << reader) model
        , onFocus config.onFocus
        , onBlur config.onBlur
        , id slug
        , name slug
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
    ]


renderAutocomplete : model -> AutocompleteConfig model msg -> List (Validation model) -> List (Html msg)
renderAutocomplete model ({ filterReader, filterTagger, choiceReader, choiceTagger, slug, label, isOpen, noResults, attrs, options } as config) validations =
    let
        valid =
            validate model (FormFieldAutocompleteConfig config validations)

        pristine =
            (not << validate model) (FormFieldAutocompleteConfig config [ NotEmpty "" ])

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
             , onFocus config.onFocus
             , onBlur config.onBlur
             , valueAttr
             , id slug
             , name slug
             , classList
                [ ( "a-form__field__input", True )
                , ( "is-valid", valid )
                , ( "is-invalid", not valid )
                , ( "is-pristine", pristine )
                , ( "is-touched", not pristine )
                ]
             ]
                ++ attrs
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

    NotEmpty "This field cannot be empty."
    Expression (Regex.regex "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$") "Insert a valid email."
    Custom (\model -> always True) "This error message will never be shown."

-}
type Validation model
    = NotEmpty String
    | Expression Regex.Regex String
    | Custom (model -> Bool) String


{-| Validate a `FormField`.

    isValid model usernameConfig

-}
isValid : model -> FormField model msg -> Bool
isValid model (FormField opaqueConfig) =
    validate model opaqueConfig


validate : model -> FormFieldConfig model msg -> Bool
validate model opaqueConfig =
    List.all (validateRule model opaqueConfig) (pickValidationRules opaqueConfig)


validateRule : model -> FormFieldConfig model msg -> Validation model -> Bool
validateRule model config validation =
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

        ( NotEmpty _, FormFieldDatepickerConfig { reader } _ ) ->
            (not << isEmpty << Maybe.withDefault "" << Maybe.map toString << reader) model

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

        FormFieldPasswordConfig _ validations ->
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
            if validateRule model opaqueConfig rule then
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
