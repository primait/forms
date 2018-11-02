module Prima.DatePicker exposing (Model, Msg(..), init, selectedDate, update, view)

{-|

@docs init, Model, Msg, update, view, selectedDate

-}

import Date exposing (Date, Day(..), Month(..), day, dayOfWeek, month, year)
import Date.Extra.Compare as DateCompare
import Date.Extra.Config exposing (Config)
import Date.Extra.Config.Config_en_gb exposing (config)
import Date.Extra.Core exposing (daysInMonth, intToMonth, isoDayOfWeek, toFirstOfMonth)
import Date.Extra.Create exposing (dateFromFields)
import Date.Extra.Duration as Duration
import Date.Extra.Field as Field
import Date.Extra.Format as DateFormat
import Date.Extra.Utils exposing (dayList)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


{-| -}
type alias Model =
    { date : Date
    , selectingYear : Bool
    , daysPickerRange : ( Date, Date )
    }


{-| Get initial time picker model
-}
init : Date -> ( Date, Date ) -> Model
init date daysRange =
    { date = adjustInitialDate date daysRange
    , selectingYear = False
    , daysPickerRange = daysRange
    }


{-| Returns currently selected date
-}
selectedDate : Model -> Date
selectedDate model =
    model.date


adjustInitialDate : Date -> ( Date, Date ) -> Date
adjustInitialDate day ( low, high ) =
    if DateCompare.is3 DateCompare.BetweenOpen day low high then
        day
    else
        low


dateFormatConfig : Config -> Config
dateFormatConfig ({ i18n, format } as config) =
    { config
        | i18n =
            { i18n
                | dayShort = String.left 3 << formatDay
                , dayName = formatDay
                , monthShort = String.left 3 << formatMonth
                , monthName = formatMonth
            }
    }


formattedDay : Model -> String
formattedDay model =
    DateFormat.format (dateFormatConfig config) "%a, %b %-d" model.date


formattedMonth : Model -> String
formattedMonth model =
    DateFormat.format (dateFormatConfig config) "%B %Y" <| model.date


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
            updateSelectedMonth model (Duration.add Duration.Month -1 model.date)

        NextMonth ->
            updateSelectedMonth model (Duration.add Duration.Month 1 model.date)

        SelectYear year ->
            case Field.fieldToDate (Field.Year year) model.date of
                Just date ->
                    updateSelectedYear model date

                Nothing ->
                    model

        SelectDay day ->
            case Field.fieldToDate (Field.DayOfMonth day) model.date of
                Just date ->
                    updateSelectedDay model date

                Nothing ->
                    model


fromDateRangeToList : ( Date, Date ) -> List Date
fromDateRangeToList ( high, low ) =
    let
        diff =
            Duration.diffDays high (Duration.add Duration.Day 1 low)
    in
    dayList diff low


updateSelectedYear : Model -> Date -> Model
updateSelectedYear model day =
    let
        ( low, high ) =
            model.daysPickerRange

        year =
            Date.year day

        lowYear =
            Date.year low

        highYear =
            Date.year high
    in
    if DateCompare.is3 DateCompare.BetweenOpen day low high then
        { model | date = day, selectingYear = False }
    else if lowYear <= year && year <= highYear then
        { model | date = day, selectingYear = False }
    else
        { model | selectingYear = False }


updateSelectedMonth : Model -> Date -> Model
updateSelectedMonth model day =
    let
        ( low, high ) =
            model.daysPickerRange
    in
    if DateCompare.is3 DateCompare.BetweenOpen day low high then
        { model | date = day }
    else if DateCompare.is DateCompare.SameOrAfter day high then
        { model | date = high }
    else
        { model | date = low }


updateSelectedDay : Model -> Date -> Model
updateSelectedDay model day =
    let
        ( low, high ) =
            model.daysPickerRange
    in
    if DateCompare.is3 DateCompare.BetweenOpen day low high then
        { model | date = day }
    else
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
            , onClick
                (if selectingYear then
                    DaySelection
                 else
                    YearSelection
                )
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
        (List.map
            (\day ->
                span [] [ text day ]
            )
            [ "Lun", "Mar", "Mer", "Gio", "Ven", "Sab", "Dom" ]
        )


monthDays : Model -> Html Msg
monthDays ({ date, daysPickerRange } as model) =
    let
        currentYear =
            Date.year date

        currentMonth =
            Date.month date

        ( lowDate, highDate ) =
            daysPickerRange

        daysCount =
            daysInMonth currentYear currentMonth

        weekDay =
            (isoDayOfWeek << dayOfWeek << toFirstOfMonth) date

        leftPadding =
            weekDay - 1

        rightPadding =
            35 - daysCount - leftPadding

        weeks =
            chunks 7 (List.repeat leftPadding 0 ++ List.range 1 daysCount ++ List.repeat rightPadding 0)

        firstOfMonth =
            dateFromFields currentYear currentMonth 1 0 0 0 0

        lastOfMonth =
            dateFromFields currentYear currentMonth daysCount 0 0 0 0

        lowDayInMonth =
            if DateCompare.is DateCompare.SameOrAfter lowDate firstOfMonth then
                lowDate
            else
                firstOfMonth

        highDayInMonth =
            if DateCompare.is DateCompare.SameOrAfter highDate lastOfMonth then
                lastOfMonth
            else
                highDate

        availableDays =
            (List.map Date.day << List.reverse << fromDateRangeToList) ( lowDayInMonth, highDayInMonth )

        disabledDaysInMonth =
            (List.filter (not << flip List.member availableDays) << List.range 1) daysCount
    in
    div
        [ class "a-datepicker__picker__monthDays"
        ]
        (List.map (\week -> weekRow week (Date.day date) disabledDaysInMonth) weeks)


weekRow : List Int -> Int -> List Int -> Html Msg
weekRow days currentDay disabledDays =
    div
        [ class "a-datepicker__picker__days" ]
        (List.map (\day -> dayCell day currentDay (List.member day disabledDays)) days)


dayCell : Int -> Int -> Bool -> Html Msg
dayCell dayNumber currentDay disabled =
    if dayNumber > 0 then
        div
            [ classList
                [ ( "a-datepicker__picker__days__item", True )
                , ( "is-selected", dayNumber == currentDay )
                , ( "is-disabled", disabled )
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
yearPicker ({ daysPickerRange } as model) =
    let
        ( lowerBound, upperBound ) =
            daysPickerRange
    in
    div
        [ class "a-datepicker__yearPicker" ]
        [ div
            [ class "a-datepicker__yearPicker__scroller" ]
            [ div
                [ class "a-datepicker__yearPicker__scroller__list" ]
                (List.map (\y -> yearButton y (year model.date)) <| List.range (Date.year lowerBound) (Date.year upperBound))
            ]
        ]


yearButton : Int -> Int -> Html Msg
yearButton year currentYear =
    span
        [ classList
            [ ( "a-datepicker__yearPicker__scroller__list__item", True )
            , ( "is-selected", year == currentYear )
            ]
        , (onClick << SelectYear) year
        ]
        [ (text << toString) year
        ]


chunks : Int -> List a -> List (List a)
chunks k xs =
    if List.length xs > k then
        List.take k xs :: chunks k (List.drop k xs)
    else
        [ xs ]


formatDay : Day -> String
formatDay day =
    case day of
        Mon ->
            "Lunedì"

        Tue ->
            "Martedì"

        Wed ->
            "Mercoledì"

        Thu ->
            "Giovedì"

        Fri ->
            "Venerdì"

        Sat ->
            "Sabato"

        Sun ->
            "Domenica"


formatMonth : Month -> String
formatMonth month =
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
