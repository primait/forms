module FormApp exposing (..)

import Date exposing (Date, Day(..), Month(..))
import Date.Format
import DatePicker exposing (DatePicker)
import Html exposing (..)
import Html.Attributes exposing (..)
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
import Task
import Tuple


type alias Model =
    { userName : Maybe String
    , note : Maybe String
    , gender : Maybe String
    , city : Maybe String
    , isOpenCity : Bool
    , privacy : Bool
    , dateOfBirth : Maybe Date
    , dateOfBirthDP : Maybe DatePicker
    , country : Maybe String
    , countryFilter : Maybe String
    , isOpenCountry : Bool
    , visitedCountries : List ( Label, Slug, Bool )
    }


type alias Label =
    String


type alias Slug =
    String


initialModel : Model
initialModel =
    Model
        Nothing
        Nothing
        Nothing
        Nothing
        False
        False
        Nothing
        Nothing
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
    | UserName
    | City
    | DateOfBirth
    | Country
    | VisitedCountries
    | Note


type Msg
    = UpdateField FieldName (Maybe String)
    | UpdateAutocomplete FieldName (Maybe String)
    | UpdateDate FieldName DatePicker.Msg
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


fetchDateToday : Cmd Msg
fetchDateToday =
    Task.perform FetchDateToday Date.now


init : ( Model, Cmd Msg )
init =
    let
        ( dateOfBirthDP, dpCmd ) =
            DatePicker.init
    in
    { initialModel
        | dateOfBirthDP = Just dateOfBirthDP
    }
        ! [ fetchDateToday
          , Cmd.map (UpdateDate DateOfBirth) dpCmd
          ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDateToday date ->
            { model | dateOfBirth = Just date } ! []

        UpdateField UserName value ->
            { model | userName = value } ! []

        UpdateField Note value ->
            { model | note = value } ! []

        UpdateField Gender value ->
            { model | gender = value } ! []

        UpdateField City value ->
            { model | city = value } ! []

        UpdateFlag Privacy value ->
            { model | privacy = value } ! []

        UpdateField Country value ->
            { model | country = value, countryFilter = Nothing, isOpenCountry = False } ! []

        UpdateDate DateOfBirth dpMsg ->
            let
                ( dateOfBirthInitialDP, _ ) =
                    DatePicker.init

                ( updatedDP, dpCmd, dateEvent ) =
                    DatePicker.update
                        datepickerSettings
                        dpMsg
                        (case model.dateOfBirthDP of
                            Just dateOfBirthDP ->
                                dateOfBirthDP

                            Nothing ->
                                dateOfBirthInitialDP
                        )

                date =
                    case dateEvent of
                        DatePicker.NoChange ->
                            model.dateOfBirth

                        DatePicker.Changed chosenDate ->
                            chosenDate
            in
            { model
                | dateOfBirth = date
                , dateOfBirthDP = Just updatedDP
            }
                ! [ Cmd.map (UpdateDate DateOfBirth) dpCmd ]

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


userNameConfig : FormField Model Msg
userNameConfig =
    Form.textConfig
        "user_name"
        "User name"
        False
        [ minlength 3, maxlength 12 ]
        .userName
        (UpdateField UserName)
        Focus
        Blur
        Nothing
        [ NotEmpty "Empty value is not acceptable."
        , Custom ((<=) 3 << String.length << Maybe.withDefault "" << .userName) "Value must be between 3 and 12 characters length."
        ]


noteConfig : FormField Model Msg
noteConfig =
    Form.textareaConfig
        "note"
        "Note"
        False
        []
        .note
        (UpdateField Note)
        Focus
        Blur
        Nothing
        [ NotEmpty "Empty value is not acceptable." ]


genderConfig : FormField Model Msg
genderConfig =
    Form.radioConfig
        "gender"
        "Gender"
        False
        []
        .gender
        (UpdateField Gender)
        Focus
        Blur
        [ RadioOption "Male" "male"
        , RadioOption "Female" "female"
        ]
        Nothing
        [ Custom ((==) "female" << Maybe.withDefault "female" << .gender) "You must select `Female` to proceed." ]


privacyConfig : FormField Model Msg
privacyConfig =
    Form.checkboxConfig
        "privacy"
        "Privacy"
        False
        []
        .privacy
        (UpdateFlag Privacy)
        Focus
        Blur
        Nothing
        []


visitedCountriesConfig : Model -> FormField Model Msg
visitedCountriesConfig { visitedCountries } =
    Form.checkboxWithOptionsConfig
        "visited_countries"
        "Visited countries"
        False
        []
        (List.map (\( label, slug, checked ) -> ( slug, checked )) << .visitedCountries)
        (UpdateCheckbox VisitedCountries)
        Focus
        Blur
        (List.map (\( label, slug, checked ) -> CheckboxOption label slug checked) visitedCountries)
        Nothing
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
        Nothing
        [ NotEmpty "Empty value is not acceptable." ]


dateOfBirthConfig : DatePicker -> FormField Model Msg
dateOfBirthConfig datepicker =
    Form.datepickerConfig
        "date_of_birth"
        "Date of Birth"
        False
        .dateOfBirth
        (UpdateDate DateOfBirth)
        datepicker
        datepickerSettings
        Nothing
        [ Custom (Maybe.withDefault False << Maybe.map (\_ -> True) << .dateOfBirth) "This is not a valid date." ]


countryConfig : Model -> FormField Model Msg
countryConfig { countryFilter, isOpenCountry } =
    let
        lowerFilter =
            (String.toLower << Maybe.withDefault "") countryFilter
    in
    Form.autocompleteConfig
        "country"
        "Country"
        False
        isOpenCountry
        (Just "Nessun risultato disponibile")
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
        Nothing
        [ NotEmpty "Empty value is not acceptable." ]


view : Model -> Html Msg
view model =
    div
        [ class "a-container a-container--small" ]
        [ Form.render model userNameConfig
        , Form.render model noteConfig
        , Form.render model genderConfig
        , Form.render model privacyConfig
        , Form.render model (visitedCountriesConfig model)
        , Form.render model (cityConfig model.isOpenCity)
        , renderOrNothing (Maybe.map (Form.render model << dateOfBirthConfig) model.dateOfBirthDP)
        , Form.render model (countryConfig model)
        ]


renderOrNothing : Maybe (Html a) -> Html a
renderOrNothing maybeHtml =
    Maybe.withDefault (text "") maybeHtml


formatDate : String -> Maybe Date -> String
formatDate dateFormat date =
    Maybe.map (Date.Format.format dateFormat) date |> Maybe.withDefault ""


datepickerSettings : DatePicker.Settings
datepickerSettings =
    let
        settings =
            DatePicker.defaultSettings
    in
    { settings
        | dateFormatter = formatDate "%d/%m/%Y" << Just
        , dayFormatter = dayFormatter
        , monthFormatter = monthFormatter
        , firstDayOfWeek = Mon
        , inputClassList =
            [ ( "form__field__input", True )
            , ( "form__field__input--datepicker", True )
            ]
    }


dayFormatter : Day -> String
dayFormatter day =
    case day of
        Mon ->
            "Lun"

        Tue ->
            "Mar"

        Wed ->
            "Mer"

        Thu ->
            "Gio"

        Fri ->
            "Ven"

        Sat ->
            "Sab"

        Sun ->
            "Dom"


monthFormatter : Month -> String
monthFormatter month =
    case month of
        Jan ->
            "Gennaio"

        Feb ->
            "Febbraio"

        Mar ->
            "Marzo"

        Apr ->
            "Aprile"

        May ->
            "Maggio"

        Jun ->
            "Giugno"

        Jul ->
            "Luglio"

        Aug ->
            "Agosto"

        Sep ->
            "Settembre"

        Oct ->
            "Ottobre"

        Nov ->
            "Novembre"

        Dec ->
            "Dicembre"
