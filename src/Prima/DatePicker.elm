module Prima.DatePicker exposing (Model, Msg, init, selectedDate, update, view)

{-|

@docs init, Model, Msg, update, view, selectedDate

-}

import Date exposing (Date, day, dayOfWeek, month, year)
import Date.Extra.Config.Config_en_au exposing (config)
import Date.Extra.Core exposing (daysInMonth, intToMonth, isoDayOfWeek, toFirstOfMonth)
import Date.Extra.Duration as Duration
import Date.Extra.Field as Field
import Date.Extra.Format as DateFormat
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


{-| -}
type alias Model =
    { date : Date
    , selectingYear : Bool
    }


{-| Get initial time picker model by passing date and a main color (in hex format)
-}
init : Date -> Model
init date =
    { date = date
    , selectingYear = False
    }


{-| Returns currently selected date
-}
selectedDate : Model -> Date
selectedDate model =
    model.date


formattedDay : Model -> String
formattedDay model =
    DateFormat.format config "%a, %b %-d" model.date


formattedMonth : Model -> String
formattedMonth model =
    DateFormat.format config "%B %Y" <| model.date


{-| -}
type Msg
    = Noop
    | YearSelection
    | DaySelection
    | PrevMonth
    | NextMonth
    | SelectYear Int
    | SelectDay Int


{-| -}
update : Msg -> Model -> Model
update msg model =
    case msg of
        Noop ->
            model

        YearSelection ->
            { model | selectingYear = True }

        DaySelection ->
            { model | selectingYear = False }

        PrevMonth ->
            { model | date = Duration.add Duration.Month -1 model.date }

        NextMonth ->
            { model | date = Duration.add Duration.Month 1 model.date }

        SelectYear year ->
            case Field.fieldToDate (Field.Year year) model.date of
                Just date ->
                    { model
                        | date = date
                        , selectingYear = False
                    }

                Nothing ->
                    model

        SelectDay day ->
            case Field.fieldToDate (Field.DayOfMonth day) model.date of
                Just date ->
                    { model | date = date }

                Nothing ->
                    model


{-| -}
view : Model -> Html Msg
view model =
    let
        content =
            if model.selectingYear then
                yearPicker model
            else
                picker model
    in
    div
        [ Html.Attributes.class "date-picker" ]
        [ header model
        , content
        ]


header : Model -> Html Msg
header model =
    let
        ( dayClass, yearClass ) =
            if model.selectingYear then
                ( "day", "year selected" )
            else
                ( "day selected", "year" )
    in
    div
        [ Html.Attributes.class "header"
        ]
        [ div
            [ Html.Attributes.class yearClass, onClick YearSelection ]
            [ Html.text <| toString <| year model.date
            ]
        , div
            [ Html.Attributes.class dayClass, onClick DaySelection ]
            [ Html.text <| formattedDay model
            ]
        ]


weekDays : Html Msg
weekDays =
    let
        days =
            List.map (\day -> span [] [ Html.text day ]) [ "M", "T", "W", "T", "F", "S", "S" ]
    in
    div
        [ Html.Attributes.class "week-days" ]
        days


monthDays : Model -> Html Msg
monthDays model =
    let
        daysCount =
            daysInMonth (year model.date) (month model.date)

        weekDay =
            isoDayOfWeek <| dayOfWeek <| toFirstOfMonth <| model.date

        leftPadding =
            weekDay - 1

        rightPadding =
            35 - daysCount - leftPadding

        weeks =
            chunks 7 (List.repeat leftPadding 0 ++ List.range 1 daysCount ++ List.repeat rightPadding 0)

        rows =
            List.map (\week -> weekRow week (day model.date)) weeks
    in
    div
        [ Html.Attributes.class "month-days" ]
        [ div
            [ Html.Attributes.class "day-rows" ]
            rows
        ]


weekRow : List Int -> Int -> Html Msg
weekRow days currentDay =
    div
        [ Html.Attributes.class "days-row" ]
        (List.map (\day -> dayCell day currentDay) days)


dayCell : Int -> Int -> Html Msg
dayCell dayNumber currentDay =
    if dayNumber > 0 then
        let
            backgroundClass =
                if dayNumber == currentDay then
                    "day-background selected"
                else
                    "day-background"
        in
        button
            [ Html.Attributes.class "day", onClick <| SelectDay dayNumber ]
            [ div
                [ Html.Attributes.class backgroundClass
                ]
                []
            , span [ Html.Attributes.class "day-number" ] [ Html.text (toString dayNumber) ]
            ]
    else
        div
            [ Html.Attributes.class "empty-day" ]
            []


picker : Model -> Html Msg
picker model =
    div
        [ Html.Attributes.class "picker" ]
        [ div
            [ Html.Attributes.class "month-year-selector" ]
            [ button
                [ Html.Attributes.class "navigation-wrapper", onClick PrevMonth ]
                []
            , div
                [ Html.Attributes.class "month-year" ]
                [ Html.text <| formattedMonth model
                ]
            , button
                [ Html.Attributes.class "navigation-wrapper", onClick NextMonth ]
                []
            ]
        , weekDays
        , monthDays model
        ]


yearPicker : Model -> Html Msg
yearPicker model =
    let
        yearButtons =
            List.map (\y -> yearButton y (year model.date)) <| List.range 1917 2117
    in
    div
        [ Html.Attributes.class "year-picker" ]
        [ div
            [ Html.Attributes.class "year-list-wrapper" ]
            [ div
                [ Html.Attributes.class "year-list" ]
                yearButtons
            ]
        ]


yearButton : Int -> Int -> Html Msg
yearButton year currentYear =
    let
        spanClass =
            if year == currentYear then
                "selected"
            else
                ""
    in
    button
        [ Html.Attributes.class "year", onClick <| SelectYear year ]
        [ span
            [ Html.Attributes.class spanClass ]
            [ text <| toString year ]
        ]


chunks : Int -> List a -> List (List a)
chunks k xs =
    if List.length xs > k then
        List.take k xs :: chunks k (List.drop k xs)
    else
        [ xs ]
