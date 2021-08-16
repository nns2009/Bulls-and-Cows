module Main exposing (..)

import List.Extra
import Maybe exposing (withDefault)
import Random

import Browser

import Css as CSS exposing (..)
import Css.Transitions as Tr
import Css.Transitions exposing (transition)
import Css.Global as GSS exposing (children)
-- import Html
import Html.Styled as H
import Html.Styled exposing (text, div, span, p, th, tr, td, ul, li, input, button, form, Html, h1, toUnstyled)
import Html.Styled.Attributes as Attr
import Html.Styled.Attributes exposing (id, class, value, type_, css)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Debug exposing (toString)
import Css.Global exposing (descendants)

-- Common
pluralize : String -> Int -> String
pluralize string count =
    if count == 1 then
        string
    else
        string ++ "s"

nThings : String -> Int -> String
nThings string count =
    String.fromInt count ++ " " ++ pluralize string count

pluralizeIs : Int -> String
pluralizeIs count =
    if count == 1 then
        "is"
    else
        "are"

-- Game Logic

main : Program () Model Msg
main = Browser.document
    { init = init
    , update = update
    , view = view
    , subscriptions = (\_ -> Sub.none)
    }

digitsCount = 4
type alias Model =
    { answer: Maybe (List Int)
    , message: String
    , guess: String
    , guesses: List (List Int)
    }

generateAnswer =
    Random.generate Answer (Random.list digitsCount (Random.int 0 9))

init : flags -> (Model, Cmd Msg)
init _ =
    ( { answer = Nothing
      , message = ""
      , guess = ""
      , guesses = []
      }
    , generateAnswer
    )

type Msg
    = Answer (List Int)
    | GuessChange String
    | GuessCheck
    | Restart

stringToDigits : String -> List Int
stringToDigits s =
    s |> String.toList |> List.map (String.fromChar >> String.toInt >> withDefault -1)

digitsToString : List Int -> String
digitsToString digits =
    digits |> List.map String.fromInt |> String.join ""

type alias BullsCows =
    { bulls: Int
    , cows: Int
    }

countBullsCows : List Int -> List Int -> BullsCows
countBullsCows digitsA digitsB =
    let digits = List.Extra.zip digitsA digitsB
        bulls = digits
            |> List.filter
                (\p ->
                    let (a, b) = p
                    in a == b
                )
            |> List.length
        cows =
            (digitsA |> List.Extra.count
              (\d -> List.member d digitsB))
            - bulls
    in { bulls = bulls , cows = cows }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Answer digits ->
            if (digits |> List.Extra.unique |> List.length) == digitsCount then
                ( { model | answer = Just digits }
                , Cmd.none
                )
            else
                ( model
                , generateAnswer
                )
        GuessChange newGuess ->
            let newGuessCleaned =
                  newGuess |> String.filter Char.isDigit
            in
                ( { model | guess = newGuessCleaned }
                , Cmd.none
                )
        GuessCheck ->
            let guessParsed = model.guess |> stringToDigits
            in
                ( { model 
                  | guesses = model.guesses ++ [guessParsed]
                  , guess = ""
                  }
                , Cmd.none
                )
        Restart ->
            init ()

debugBorder = batch
    [ --border3 (px 1) solid (hsl 0 0 0.95)
    ]

centerChildren = batch
    [ displayFlex
    , alignItems center
    , justifyContent center
    ]

dockFull = batch
    [ position absolute
    , top zero
    , bottom zero
    , right zero
    , left zero
    ]

dockRight = batch
    [ position absolute
    , right zero
    , top zero
    , bottom zero
    ]
dockLeft = batch
    [ position absolute
    , left zero
    , top zero
    , bottom zero
    ]
dockTop = batch
    [ position absolute
    , top zero
    , left zero
    , right zero
    ]

-- Text styles
mainFontFamily = "Calibri"
mainFontSize = px 24

baseTextStyle = batch
    [ fontFamilies [ mainFontFamily ]
    ]

mainTextStyle = batch
    [ baseTextStyle
    , fontSize mainFontSize 
    ]

secondaryTextStyle = batch
    [ baseTextStyle
    , color (hsl 0 0 0.4)
    ]

listStyle = batch
    [ paddingLeft (em 1)
    , marginTop (px 10), marginBottom (px 10) ]

rowHeight = 50
rowStyle = batch
    [ minHeight zero
    , height (px rowHeight)
    ]
cellStyle = batch
    [ minHeight zero
    , border3 (px 1) solid (rgb 150 150 150)
    , textAlign center
    ]
headerCellStyle = batch
    [ cellStyle
    , fontSize (em 0.7)
    ]

rulesPanelOpenTime = 360
rulesClosedWidth = px 100
rulesOpenWidth = px 290
rulesTitleTranisionDistance = 18
rulesPanelPadding = em 1
rulesTextStyle = batch
    [ fontSize (em 1.2)
    , fontFamilies [ mainFontFamily ]
    ]


-- Debug styles
debugBackgroundColor = rgb 255 200 200

guessView : List Int -> List Int -> Html Msg
guessView answer guess =
    let 
        { bulls, cows } = countBullsCows guess answer
        guessString = String.join "" (guess |> List.map String.fromInt)
    in
    tr [ css [ rowStyle ] ]
      [ td [ css [cellStyle] ] [text guessString]
      , td [ css [cellStyle] ] [text <| String.fromInt cows]
      , td [ css [cellStyle] ] [text <| String.fromInt bulls]
      ]
      --  [ text (guessString ++ " - " ++ (String.fromInt cows) ++ " cows, " ++ (String.fromInt bulls) ++ " bulls")]




rulesView : Html Msg
rulesView =
  div [ css
    [ dockRight
    , zIndex (int 4)
    , overflow hidden
    , backgroundColor (hsl 0 0 0.985)
    , rulesTextStyle
    , transition
      [ Tr.width rulesPanelOpenTime
      ]
    , width rulesClosedWidth
    , descendants
      [ GSS.class "revealHeader"
        [ opacity (num 1)
        , visibility visible
        , padding rulesPanelPadding
        , transition
            [ Tr.opacity rulesPanelOpenTime
            , Tr.visibility rulesPanelOpenTime
            ]
        , centerChildren
        ]
      , GSS.class "revealContent"
        [ opacity (num 0)
        , padding rulesPanelPadding
        , width rulesOpenWidth
        , transition
            [ Tr.opacity rulesPanelOpenTime
            ]
        , centerChildren 
        ]
      , GSS.class "revealHeaderTitle"
        [ top (px 0)
        , transition
            [ Tr.top rulesPanelOpenTime ]
        ]
      , GSS.class "revealContentTitle"
        [ top (px rulesTitleTranisionDistance)
        , transition 
            [ Tr.top rulesPanelOpenTime ]
        ]
      ]
    , hover
    [ width rulesOpenWidth
    , descendants
        [ GSS.class "revealHeader"
            [ opacity (num 0)
            , visibility hidden
            ]
        , GSS.class "revealContent"
            [ opacity (num 1)
            ]
        , GSS.class "revealHeaderTitle"
            [ top (px <| negate rulesTitleTranisionDistance) ]
        , GSS.class "revealContentTitle"
            [ top (px 0) ]
        ]
    ]
    ]]
    [ div
      [ class "revealHeader"
      , css
        [ dockFull
        , secondaryTextStyle
        , fontSize (px 20)
        ]
      ]
      [ div [ class "revealHeaderTitle", css [ position relative ] ]
          [ text "Rules" ]
      ]
    , div [ class "revealContent", css [height (pct 100)] ]
      [ div []
        [ div [ class "revealContentTitle", css [ textAlign center, position relative ] ]
          [ H.b [] [ text "Rules" ] ]
        , p [] [ text "Computer picked up a random number (with a specified number of digits) where all digits are different - your goal is to guess it" ]
        , p [] [ text "Your guess has to be a number with the same number of digits and digits in your number should be different as well. For each of your guesses computer replies with a number of \"Cows\" and \"Bulls\", where"]
        , ul [ css [listStyle] ]
            [ li [] [ text "\"Cows\" means the number of correct digits but in the wrong position"]
            , li [] [ text "\"Bulls\" means the number of correct digits in the correct position"]
            ]
        , p [] [ text "Try to guess computer's number in the fewest moves possible!" ]
        , p [] [ text "Post your results in the comments:)" ]
        ]
      ]
    ]

-- Text pluralization
nCows = nThings "cow"
nBulls = nThings "bull"
nDigits = nThings "digit"

welcomePrompt =
    [ div [] [ text <| "I picked " ++ (toString digitsCount) ++ " different random digits" ]
    , div [] [ text <| "Try to guess them!" ]
    ]

firstGuessPrompt : List Int -> BullsCows -> List (Html Msg)
firstGuessPrompt guess res =
    [ div [] [ text (digitsToString guess ++ "?")]
    , div [] [ text "Good guess!" ]
    , div [] [ text <| "My response is " ++ nCows res.cows ++ " and " ++ nBulls res.bulls ++ ", which means there " ++ pluralizeIs res.cows ]
    , ul [ css [listStyle] ]
        [ li [] [ text <| nDigits res.cows ++ " that you guessed correctly but in the wrong position and" ]
        , li [] [ text <| nDigits res.bulls  ++ " that you guessed correctly and in the correct position" ]
        ]
    , div [] [ text <| "Pick your next guess ..."]
    ]

secondGuessPrompt : List Int -> BullsCows -> List (Html Msg)
secondGuessPrompt guess res =
    let { cows, bulls } = res
        correct = cows + bulls
    in
        [ div [] [ text (digitsToString guess ++ "? - Interesting")]
        , div [] [ text <| nCows cows ++ " and " ++ nBulls bulls ++ ", which as before means there " ++ pluralizeIs correct ++ " " ++ String.fromInt cows ++ "+" ++ String.fromInt bulls ++ "=" ++ String.fromInt correct ++ " " ++ pluralize "digit" correct ++ " that you guessed correctly:" ]
        , ul [ css [listStyle] ]
            [ li [] [ text <| nDigits cows ++ " in the wrong position and" ]
            , li [] [ text <| nDigits bulls ++ " in the correct one"]
            ]
        , div [] [ text "Continue guessing ..." ]
        ]

thirdGuessPrompt : List Int -> BullsCows -> List (Html Msg)
thirdGuessPrompt guess res =
    let { cows, bulls } = res
        --correct = cows + bulls
    in
        [ div [] [ text <| digitsToString guess ++ " - " ++ nCows cows ++ " and " ++ nBulls bulls ]
        , ul [ css [listStyle] ]
            [ li [] [ text <| nDigits cows ++ " in wrong position" ]
            , li [] [ text <| nDigits bulls ++ " in correct position" ]
            ]
        , div [] [ text "Keep guessing ..." ]
        ]

fourthGuessPrompt : List Int -> BullsCows -> List (Html Msg)
fourthGuessPrompt guess res =
    let { cows, bulls } = res
        --correct = cows + bulls
    in
        [ div [] [ text <| digitsToString guess ++ " - " ++ nCows cows ++ " and " ++ nBulls bulls ]
        , div [] [ text "Continue guessing until you guess the correct number" ]
        ]

goodLuckPrompt : List (Html Msg)
goodLuckPrompt =
    [ div [] [ text "Good luck!" ]
    , div [] [ text "Feel free to refer to the rules at the right side if needed" ]
    ]



propmtView : List Int -> Int -> Maybe (List Int) -> Html Msg
propmtView answer numGuesses lastGuess =
    let
        show = numGuesses <= 5
        res = countBullsCows answer (lastGuess |> withDefault [])
    in
        div [ css 
            [ position absolute
            , top zero
            , width (pct 50)
            , height (pct 40)
            , pointerEvents none
            , centerChildren
            , mainTextStyle
            , opacity <| num <| if show then 1 else 0
            , transition
            [ Tr.opacity 300 ]
            ]]
            [ div []
                (case numGuesses of
                  0 -> welcomePrompt
                  1 -> firstGuessPrompt (lastGuess |> withDefault []) res
                  2 -> secondGuessPrompt (lastGuess |> withDefault []) res
                  3 -> thirdGuessPrompt (lastGuess |> withDefault []) res
                  4 -> fourthGuessPrompt (lastGuess |> withDefault []) res
                  5 -> goodLuckPrompt
                  _ ->
                    []
                )
            ]

view : Model -> Browser.Document Msg
view model =
    let answerText =
          case model.answer of
            Nothing ->
                "No digits generated so far"
            Just digits ->
                String.join "" (List.map toString digits)
        numGuesses = List.length model.guesses
        lastGuess = List.Extra.last model.guesses
        gameOver = 
            case (lastGuess, model.answer) of
               (Nothing, _) -> False
               (_, Nothing) -> False
               (Just guess, Just answerDigits) -> guess == answerDigits
    in
        { title = answerText
        , body =
            [ div [css [centerChildren, height (pct 100)]]
                [ GSS.global
                  [ GSS.html [ height (pct 100) ]
                  , GSS.body [ height (pct 100) ]
                  , GSS.everything [ boxSizing borderBox ]
                  , GSS.p [ marginTop (px 10), marginBottom (px 10) ]
                  ]
                , div [css
                    [ width (px 230)
                    , debugBorder
                    , displayFlex
                    , flexDirection column
                    ]]
                    [ div [ css
                      [ overflow hidden
                      , height (px <| rowHeight * (toFloat <| if numGuesses == 0 then 0 else (numGuesses + 1)) + 1)
                      , transition
                        [ Tr.height 150 ]
                      ]]
                      [ H.table [ css
                            [ width (pct 100)
                            , mainTextStyle
                            , borderCollapse collapse
                            ]]
                            <| tr [ css [ rowStyle ] ]
                            [ th [ css [headerCellStyle, width (pct 50) ] ] [ text "Guess" ]
                            , th [ css [headerCellStyle, width (pct 25) ] ] [ text "Cows" ]
                            , th [ css [headerCellStyle, width (pct 25) ] ] [ text "Bulls" ]
                            ]
                            :: (model.guesses |> List.map (guessView (model.answer |> withDefault [])))
                      ]
                    , form 
                        [ onSubmit GuessCheck
                        , css
                          [ if gameOver then display none else displayFlex
                          , position relative
                          ]
                        ]
                        [ input
                            [ value model.guess
                            , onInput GuessChange
                            --, 
                            , css
                                [ flex2 (num 1) (num 1)
                                , minWidth (px 0)
                                , padding4 (em 0.4) zero (em 0.4) (em 0.45)
                                , border zero
                                , borderBottom3 (px 2) solid (hsl 0 0 0.5)
                                , outline none
                                , mainTextStyle
                                ]
                            ] []
                        , button
                            [ Attr.disabled
                                (  (String.length model.guess /= digitsCount)
                                || ((model.guess |> String.toList |> List.Extra.unique |> List.length) /= digitsCount)
                                || gameOver
                                )
                            , css
                            [ mainTextStyle
                            , dockRight
                            , width (em 1.6)
                            , backgroundColor transparent
                            , border zero
                            ]
                            ] [ text "?" ] ]
                    , div [ css [mainTextStyle] ]
                        (if gameOver then
                            [ div [] [ text "Correct!" ]
                            , div [] [ text ("The number is " ++ answerText)]
                            , div [] [ text ("You guessed it in " ++ (String.fromInt <| List.length model.guesses) ++ " turns") ]
                            , button [ onClick Restart, css [ mainTextStyle ] ] [ text "Play again"]
                            ]
                         else []
                        )
                    , div []
                        [ text model.message ]
                    ]
                , rulesView
                , propmtView (model.answer |> withDefault []) numGuesses lastGuess
                ] |> toUnstyled
            ]
        }
