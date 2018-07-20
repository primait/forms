module FormApp exposing (..)

-- import DatePicker exposing (DatePicker)

import Date exposing (Date, Day(..), Month(..))
import Date.Format
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
    { username : Maybe String
    , password : Maybe String
    , note : Maybe String
    , gender : Maybe String
    , city : Maybe String
    , isOpenCity : Bool
    , privacy : Bool
    , dateOfBirth : Maybe Date

    -- , dateOfBirthDP : Maybe DatePicker
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
        Nothing
        False
        False
        Nothing
        Nothing
        -- Nothing
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
      -- | UpdateDate FieldName DatePicker.Msg
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
    initialModel
        ! [ fetchDateToday

          -- , Cmd.map (UpdateDate DateOfBirth) dpCmd
          ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchDateToday date ->
            { model | dateOfBirth = Just date } ! []

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

        UpdateFlag Privacy value ->
            { model | privacy = value } ! []

        UpdateField Country value ->
            { model | country = value, countryFilter = Nothing, isOpenCountry = False } ! []

        -- UpdateDate DateOfBirth dpMsg ->
        --     model ! []
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
        [ NotEmpty "Empty value is not acceptable." ]



-- dateOfBirthConfig : DatePicker -> FormField Model Msg
-- dateOfBirthConfig datepicker =
--     Form.datepickerConfig
--         "date_of_birth"
--         "Date of Birth"
--         .dateOfBirth
--         (UpdateDate DateOfBirth)
--         datepicker
--         datepickerSettings
--         [ Custom (Maybe.withDefault False << Maybe.map (\_ -> True) << .dateOfBirth) "This is not a valid date." ]


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
        [ NotEmpty "Empty value is not acceptable." ]


view : Model -> Html Msg
view model =
    div
        [ class "wrapper" ]
        [ node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "https://d3be8952cnveif.cloudfront.net/css/pyxis-1.0.1.css" ] []
        , Form.wrapper <| Form.render model usernameConfig
        , Form.wrapper <| Form.render model passwordConfig
        , Form.wrapper <| Form.render model noteConfig
        , Form.wrapper <| Form.render model genderConfig
        , Form.wrapper <| Form.render model privacyConfig
        , Form.wrapper <| Form.render model (visitedCountriesConfig model)
        , Form.wrapper <| Form.render model (cityConfig model.isOpenCity)

        -- , Form.wrapper <| renderOrNothing (Maybe.map (Form.render model << dateOfBirthConfig) model.dateOfBirthDP)
        , Form.wrapper <| Form.render model (countryConfig model)
        ]


renderOrNothing : Maybe (Html a) -> Html a
renderOrNothing maybeHtml =
    Maybe.withDefault (text "") maybeHtml


formatDate : String -> Maybe Date -> String
formatDate dateFormat date =
    Maybe.map (Date.Format.format dateFormat) date |> Maybe.withDefault ""
