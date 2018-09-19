module FormApp exposing (FieldName(..), Label, Model, Msg(..), Slug, cityConfig, countryConfig, dateOfBirthConfig, formatDate, genderConfig, init, initialDate, initialModel, main, noteConfig, passwordConfig, privacyConfig, renderOrNothing, update, usernameConfig, view, visitedCountriesConfig)

import Date exposing (Date, Day(..), Month(..))
import Date.Extra.Core exposing (intToMonth)
import Date.Extra.Create exposing (dateFromFields)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (..)
import Prima.DatePicker as DatePicker
import Prima.Form as Form
    exposing
        ( AutocompleteOption
        , CheckboxOption
        , FormField
        , FormFieldConfig
        , RadioOption
        , SelectOption
        , Validation(..)
        )


type alias Model =
    { username : Maybe String
    , password : Maybe String
    , note : Maybe String
    , gender : Maybe String
    , city : Maybe String
    , isOpenCity : Bool
    , privacy : Bool
    , dateOfBirth : Maybe String
    , dateOfBirthDP : DatePicker.Model
    , isVisibleDP : Bool
    , country : Maybe String
    , countryFilter : Maybe String
    , isOpenCountry : Bool
    , visitedCountries : List ( Label, Slug, Bool )
    }


type alias Label =
    String


type alias Slug =
    String


initialDate : Date
initialDate =
    dateFromFields 2018 (intToMonth 1) 1 0 0 0 0


lowDate : Date
lowDate =
    dateFromFields 2018 (intToMonth 9) 15 0 0 0 0


highDate : Date
highDate =
    dateFromFields 2018 (intToMonth 10) 28 0 0 0 0


initialModel : Model
initialModel =
    Model
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing
        False
        False
        Nothing
        (DatePicker.init initialDate ( 2018, 2050 ) (Just ( lowDate, highDate )))
        True
        Nothing
        Nothing
        False
        [ ( "Italy", "ITA", False )
        , ( "France", "FRA", False )
        , ( "U.S.A", "USA", False )
        , ( "Great Britain", "GB", False )
        ]


type FieldName
    = Privacy
    | Gender
    | Username
    | Password
    | City
    | DateOfBirth
    | Country
    | VisitedCountries
    | Note


type Msg
    = UpdateField FieldName (Maybe String)
    | UpdateAutocomplete FieldName (Maybe String)
    | UpdateDatePicker FieldName DatePicker.Msg
    | UpdateDate FieldName (Maybe Date)
    | UpdateFlag FieldName Bool
    | UpdateCheckbox FieldName Slug Bool
    | Toggle FieldName Bool
    | FetchDateToday Date
    | Focus
    | Blur


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.batch []
        }


init : ( Model, Cmd Msg )
init =
    initialModel ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField Username value ->
            { model | username = value } ! []

        UpdateField Password value ->
            { model | password = value } ! []

        UpdateField Note value ->
            { model | note = value } ! []

        UpdateField Gender value ->
            { model | gender = value } ! []

        UpdateField City value ->
            { model | city = value } ! []

        UpdateField Country value ->
            { model | country = value, countryFilter = Nothing, isOpenCountry = False } ! []

        UpdateField DateOfBirth value ->
            let
                unwrap : Maybe (Maybe a) -> Maybe a
                unwrap theMaybe =
                    case theMaybe of
                        Just something ->
                            something

                        Nothing ->
                            Nothing
            in
            { model
                | dateOfBirth = value
                , dateOfBirthDP =
                    case (unwrap << Maybe.map (Result.toMaybe << Date.fromString)) value of
                        Just date ->
                            DatePicker.init date ( 2018, 2050 ) (Just ( lowDate, highDate ))

                        _ ->
                            model.dateOfBirthDP
            }
                ! []

        UpdateFlag Privacy value ->
            { model | privacy = value } ! []

        UpdateDatePicker DateOfBirth dpMsg ->
            let
                updatedInstance =
                    DatePicker.update dpMsg model.dateOfBirthDP
            in
            { model | dateOfBirthDP = updatedInstance, dateOfBirth = (Just << Date.Format.format "%d/%m/%Y" << DatePicker.selectedDate) updatedInstance } ! []

        UpdateAutocomplete Country value ->
            { model | countryFilter = value, isOpenCountry = True } ! []

        UpdateCheckbox VisitedCountries slug isChecked ->
            { model
                | visitedCountries =
                    List.map
                        (\( optLabel, optSlug, optChecked ) ->
                            if optSlug == slug then
                                ( optLabel, optSlug, isChecked )

                            else
                                ( optLabel, optSlug, optChecked )
                        )
                        model.visitedCountries
            }
                ! []

        Toggle City isOpen ->
            { model | isOpenCity = isOpen } ! []

        Focus ->
            model ! []

        Blur ->
            model ! []

        _ ->
            model ! []


usernameConfig : FormField Model Msg
usernameConfig =
    Form.textConfig
        "user_name"
        "User name"
        [ minlength 3, maxlength 12 ]
        .username
        (UpdateField Username)
        Focus
        Blur
        False
        [ NotEmpty "Empty value is not acceptable."
        , Custom ((<=) 3 << String.length << Maybe.withDefault "" << .username) "Value must be between 3 and 12 characters length."
        ]


passwordConfig : FormField Model Msg
passwordConfig =
    Form.passwordConfig
        "password"
        "Password"
        []
        .password
        (UpdateField Password)
        Focus
        Blur
        False
        [ NotEmpty "Empty value is not acceptable."
        ]


noteConfig : FormField Model Msg
noteConfig =
    Form.textareaConfig
        "note"
        "Note"
        []
        .note
        (UpdateField Note)
        Focus
        Blur
        False
        [ NotEmpty "Empty value is not acceptable." ]


genderConfig : FormField Model Msg
genderConfig =
    Form.radioConfig
        "gender"
        "Gender"
        []
        .gender
        (UpdateField Gender)
        Focus
        Blur
        [ RadioOption "Male" "male"
        , RadioOption "Female" "female"
        ]
        False
        [ Custom ((==) "female" << Maybe.withDefault "female" << .gender) "You must select `Female` to proceed." ]


privacyConfig : FormField Model Msg
privacyConfig =
    Form.checkboxConfig
        "privacy"
        "Privacy"
        []
        .privacy
        (UpdateFlag Privacy)
        Focus
        Blur
        False
        []


visitedCountriesConfig : Model -> FormField Model Msg
visitedCountriesConfig { visitedCountries } =
    Form.checkboxWithOptionsConfig
        "visited_countries"
        "Visited countries"
        []
        (List.map (\( label, slug, checked ) -> ( slug, checked )) << .visitedCountries)
        (UpdateCheckbox VisitedCountries)
        Focus
        Blur
        (List.map (\( label, slug, checked ) -> CheckboxOption label slug checked) visitedCountries)
        False
        []


cityConfig : Bool -> FormField Model Msg
cityConfig isOpen =
    Form.selectConfig
        "city"
        "City"
        False
        isOpen
        (Just "Seleziona")
        []
        .city
        (Toggle City)
        (UpdateField City)
        Focus
        Blur
        (List.sortBy .label
            [ SelectOption "Milan" "MI"
            , SelectOption "Turin" "TO"
            , SelectOption "Rome" "RO"
            , SelectOption "Naples" "NA"
            , SelectOption "Genoa" "GE"
            ]
        )
        False
        [ NotEmpty "Empty value is not acceptable." ]


dateOfBirthConfig : Bool -> DatePicker.Model -> FormField Model Msg
dateOfBirthConfig showDatePicker datepicker =
    Form.datepickerConfig
        "date_of_birth"
        "Date of Birth"
        []
        .dateOfBirth
        (UpdateField DateOfBirth)
        (UpdateDatePicker DateOfBirth)
        Focus
        Blur
        datepicker
        showDatePicker
        False
        [ Custom (Maybe.withDefault False << Maybe.map (always True) << .dateOfBirth) "This is not a valid date." ]


countryConfig : Model -> FormField Model Msg
countryConfig { countryFilter, isOpenCountry } =
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
        (UpdateAutocomplete Country)
        (UpdateField Country)
        Focus
        Blur
        ([ AutocompleteOption "Italy" "ITA"
         , AutocompleteOption "Brasil" "BRA"
         , AutocompleteOption "France" "FRA"
         , AutocompleteOption "England" "ENG"
         , AutocompleteOption "USA" "USA"
         , AutocompleteOption "Japan" "JAP"
         ]
            |> List.filter (String.contains lowerFilter << String.toLower << .label)
        )
        False
        [ NotEmpty "Empty value is not acceptable." ]


view : Model -> Html Msg
view model =
    let
        userIcon =
            [ div
                [ class "m-form__field__group__prepend" ]
                [ i
                    [ class "a-icon a-icon-mail" ]
                    []
                ]
            ]
    in
    div
        [ class "wrapper" ]
        [ node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://d3be8952cnveif.cloudfront.net/css/pyxis-latest.css" ] []
        , Form.wrapper <| Form.render model usernameConfig
        , Form.wrapper <| Form.render model passwordConfig
        , Form.wrapper <| Form.render model noteConfig
        , Form.wrapper <| Form.render model genderConfig
        , Form.wrapper <| Form.render model privacyConfig
        , Form.wrapper <| Form.render model (visitedCountriesConfig model)
        , Form.wrapper <| Form.render model (cityConfig model.isOpenCity)
        , Form.wrapper <| Form.render model (cityConfig model.isOpenCity) ++ Form.renderWithoutLabel model (countryConfig model)
        , Form.wrapper <| Form.render model (dateOfBirthConfig model.isVisibleDP model.dateOfBirthDP)
        , Form.wrapper <| Form.renderWithGroup userIcon model usernameConfig
        ]


renderOrNothing : Maybe (Html a) -> Html a
renderOrNothing maybeHtml =
    Maybe.withDefault (text "") maybeHtml


formatDate : String -> Maybe Date -> String
formatDate dateFormat date =
    Maybe.map (Date.Format.format dateFormat) date |> Maybe.withDefault ""
