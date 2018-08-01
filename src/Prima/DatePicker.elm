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
view ({ selectingYear } as model) =
    div
        [ class "a-datepicker" ]
        [ header model
        , if selectingYear then
            yearPicker model
          else
            picker model
        ]


header : Model -> Html Msg
header ({ date, selectingYear } as model) =
    div
        [ class "a-datepicker__header"
        ]
        [ div
            [ classList
                [ ( "a-datepicker__header__year", True )
                , ( "is-selected", selectingYear )
                ]
            , onClick YearSelection
            ]
            [ (text << toString << year) date
            ]
        , div
            [ classList
                [ ( "a-datepicker__header__day", True )
                , ( "is-selected", not selectingYear )
                ]
            , onClick DaySelection
            ]
            [ (text << formattedDay) model
            ]
        ]


weekDays : Html Msg
weekDays =
    div
        [ class "a-datepicker__picker__weekDays" ]
        (List.map (\day -> span [] [ text day ]) [ "Lun", "Mar", "Mer", "Gio", "Ven", "Sab", "Dom" ])


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
    in
    div
        [ class "a-datepicker__picker__monthDays"
        ]
        (List.map (\week -> weekRow week (day model.date)) weeks)


weekRow : List Int -> Int -> Html Msg
weekRow days currentDay =
    div
        [ class "a-datepicker__picker__days" ]
        (List.map (\day -> dayCell day currentDay) days)


dayCell : Int -> Int -> Html Msg
dayCell dayNumber currentDay =
    if dayNumber > 0 then
        div
            [ classList
                [ ( "a-datepicker__picker__days__item", True )
                , ( "is-selected", dayNumber == currentDay )
                ]
            , (onClick << SelectDay) dayNumber
            ]
            [ (text << toString) dayNumber
            ]
    else
        div
            [ class "a-datepicker__picker__days__item is-empty" ]
            []


picker : Model -> Html Msg
picker model =
    div
        [ class "a-datepicker__picker" ]
        [ div
            [ class "a-datepicker__picker__header" ]
            [ span
                [ class "a-datepicker__picker__header__prevMonth", onClick PrevMonth ]
                []
            , div
                [ class "a-datepicker__picker__header__currentMonth" ]
                [ (text << formattedMonth) model
                ]
            , span
                [ class "a-datepicker__picker__header__nextMonth", onClick NextMonth ]
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
        [ class "year-picker" ]
        [ div
            [ class "year-list-wrapper" ]
            [ div
                [ class "year-list" ]
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
        [ class "year", onClick <| SelectYear year ]
        [ span
            [ class spanClass ]
            [ text <| toString year ]
        ]


chunks : Int -> List a -> List (List a)
chunks k xs =
    if List.length xs > k then
        List.take k xs :: chunks k (List.drop k xs)
    else
        [ xs ]
