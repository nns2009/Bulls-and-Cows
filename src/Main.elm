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
import Html.Styled exposing (text, div, span, a, p, th, tr, td, ul, li, input, button, form, Html, h1, toUnstyled)
import Html.Styled.Attributes as Attr
import Html.Styled.Attributes exposing (id, class, value, type_, href, css)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Debug exposing (toString)
import Css.Global exposing (descendants)

-- Common
str : Int -> String
str = String.fromInt

pluralize : String -> Int -> String
pluralize string count =
    if count == 1 then
        string
    else
        string ++ "s"

nThings : String -> Int -> String
nThings string count =
    str count ++ " " ++ pluralize string count

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
    --, message: String
    , guess: String
    , guesses: List (List Int)
    , gamesCompleted: Int
    , numGuessesAcrossAllGames: Int
    , completedTutorial: Bool
    }

generateAnswer =
    Random.generate Answer (Random.list digitsCount (Random.int 0 9))


init : flags -> (Model, Cmd Msg)
init _ = 
    ( { answer = Nothing
      --, message = ""
      , guess = ""
      , guesses = []
      , gamesCompleted = 0
      , numGuessesAcrossAllGames = 0
      , completedTutorial = False
      }
    , generateAnswer
    )

resetGame : Model -> (Model, Cmd Msg)
resetGame model =
    ( { model
      | answer = Nothing
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
    digits |> List.map str |> String.join ""

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
                gameOver = 
                    case model.answer of
                        Nothing -> False
                        Just answerDigits -> guessParsed == answerDigits
                numGuesses = List.length model.guesses + 1 -- +1 to account for the last guess
            in
                ( { model 
                  | guesses = model.guesses ++ [guessParsed]
                  , guess = ""
                  , gamesCompleted = model.gamesCompleted + (if gameOver then 1 else 0)
                  , numGuessesAcrossAllGames = model.numGuessesAcrossAllGames + 1
                  , completedTutorial = model.completedTutorial || (gameOver && numGuesses >= 5)
                  }
                , Cmd.none
                )
        Restart ->
            resetGame model

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
baseTransitionDuration = 200
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
    , fontSize (px 22)
    ]

menuTitleTextStyle = batch
    [ baseTextStyle
    , color (hsl 0 0 0.4)
    ]

linkStyle = batch
    [ baseTextStyle
    , cursor pointer
    , textDecoration none
    , color (hsl 194 1 0.42)
    , transition
      [ Tr.color baseTransitionDuration ]
    , hover
      [  color (hsl 194 1 0.24) ]
    , active
      [ color (hsl 194 1 0.16) ]
    ]

listStyle = batch
    [ paddingLeft (em 1)
    , marginTop (px 10), marginBottom (px 10)
    ]

--------------- UI style ---------------

uiTransitionDuration = 300
buttonTransitionDuration = uiTransitionDuration

buttonVerticalPadding = px 12
buttonStyle = batch
    [ mainTextStyle
    , width (pct 100)
    , paddingTop buttonVerticalPadding
    , paddingBottom buttonVerticalPadding
    , border zero
    , borderLeft3 (px 2) solid (hsl 0 0 0.3)
    , borderRight3 (px 2) solid (hsl 0 0 0.3)
    , color (hsl 0 0 0.1)
    , cursor pointer
    , transition
      [ Tr.backgroundColor buttonTransitionDuration
      , Tr.borderColor buttonTransitionDuration
      , Tr.color buttonTransitionDuration
      ]
    , backgroundColor (hsl 0 0 0.96)
    , hover
      [ backgroundColor (hsl 194 1 0.88)
      , borderColor (hsl 194 1 0.26)
      , color (hsl 194 1 0.11)
      ]
    , active
      [ backgroundColor (hsl 194 1 0.72)
      , borderColor (hsl 194 1 0.23)
      , color (hsl 194 1 0.06)
      ]
    ]

inputTransitionDuration = uiTransitionDuration
inputStyle = batch
    [ mainTextStyle
    , padding4 (em 0.4) zero (em 0.4) (em 0.45)
    , outline none
    , cursor text_
    , border zero
    , borderBottom3 (px 2) solid (hsl 0 0 0.65)
    , transition
      [ Tr.borderColor inputTransitionDuration ]
    , hover
      [ borderColor (hsl 0 0 0.52)
      ]
    , focus
      [ borderColor (hsl 0 0 0.4)
      ]
    , active
      [ borderColor (hsl 0 0 0.1)
      ]
    ]

--------------- Table style ---------------

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

--------------- Side section styles ---------------
sidePanelOpenTime = 360
sideClosedWidth = px 100
sideOpenWidth = px 290
sideTitleTranisionDistance = 18
sidePanelPadding = em 1
sideTitleStyle = batch
    [ textAlign center
    , fontWeight bold
    ]
sideTextStyle = batch
    [ fontSize (em 1.2)
    , fontFamilies [ mainFontFamily ]
    ]


--------------- Debug styles ---------------
debugBackgroundColor = rgb 255 200 200

--------------- Views ---------------

guessView : List Int -> List Int -> Html Msg
guessView answer guess =
    let 
        { bulls, cows } = countBullsCows guess answer
        guessString = String.join "" (guess |> List.map str)
    in
    tr [ css [ rowStyle ] ]
      [ td [ css [cellStyle] ] [text guessString]
      , td [ css [cellStyle] ] [text <| str cows]
      , td [ css [cellStyle] ] [text <| str bulls]
      ]
      --  [ text (guessString ++ " - " ++ (str cows) ++ " cows, " ++ (str bulls) ++ " bulls")]


--------------- Side sections ---------------

type LeftRight = Left
               | Right

sideSection : LeftRight -> String -> List (Html Msg) -> Html Msg
sideSection side title content =
  div [ css
    [ if side == Left then dockLeft else dockRight
    , zIndex (int 4)
    , overflow hidden
    , backgroundColor (hsl 0 0 0.985)
    , sideTextStyle
    , transition
      [ Tr.width sidePanelOpenTime
      ]
    , width sideClosedWidth
    , descendants
      [ GSS.class "revealHeader"
        [ opacity (num 1)
        , visibility visible
        , transition
            [ Tr.opacity sidePanelOpenTime
            , Tr.visibility sidePanelOpenTime
            ]
        ]
      , GSS.class "revealContent"
        [ opacity (num 0)
        , transition
            [ Tr.opacity sidePanelOpenTime ]
        ]
      , GSS.class "revealHeaderTitle"
        [ top (px 0)
        , transition
            [ Tr.top sidePanelOpenTime ]
        ]
      , GSS.class "revealContentTitle"
        [ top (px sideTitleTranisionDistance)
        , transition 
            [ Tr.top sidePanelOpenTime ]
        ]
      ]
    , hover
    [ width sideOpenWidth
    , descendants
        [ GSS.class "revealHeader"
            [ opacity (num 0)
            , visibility hidden
            ]
        , GSS.class "revealContent"
            [ opacity (num 1) ]
        , GSS.class "revealHeaderTitle"
            [ top (px <| negate sideTitleTranisionDistance) ]
        , GSS.class "revealContentTitle"
            [ top (px 0) ]
        ]
    ]
    ]]
    [ div
      [ class "revealHeader"
      , css
        [ dockFull
        , centerChildren
        , menuTitleTextStyle
        , fontSize (px 20)
        ]
      ]
      [ div [ class "revealHeaderTitle", css [ position relative ] ]
          [ text title ]
      ]
    , div
      [ class "revealContent"
      , css
        [ height (pct 100)
        , centerChildren 
        , padding sidePanelPadding
        , width sideOpenWidth
        ]
      ]
      [ div [] <|
            div [ class "revealContentTitle", css [ position relative, sideTitleStyle ] ]
                [ text title ]
            :: content
      ]
    ]


rulesView : Html Msg
rulesView =
    sideSection Right "Rules"
        [ p [] [ text "Computer picked up a random number (with a specified number of digits) where all digits are different - your goal is to guess it" ]
        , p [] [ text "Your guess has to be a number with the same number of digits and digits in your number should be different as well. For each of your guesses computer replies with a number of \"Cows\" and \"Bulls\", where"]
        , ul [ css [listStyle] ]
            [ li [] [ text "\"Cows\" means the number of correct digits but in the wrong position"]
            , li [] [ text "\"Bulls\" means the number of correct digits in the correct position"]
            ]
        , p [] [ text "Try to guess computer's number in the fewest moves possible!" ]
        , p [] [ text "Post your results in the comments:)" ]
        ]


menuView : Int -> Int -> Html Msg
menuView gamesCompleted numGuessesAcrossAllGames = 
    sideSection Left "Menu"
        [ p []
          [ button [ onClick Restart, css [ buttonStyle ] ] [ text "Restart" ]
          ]
        , div [ css [sideTitleStyle] ] [ text "Statistics" ]
        , p [] [ text <| "You have completed " ++ nThings "game" gamesCompleted]
        , p [] [ text <| "And made " ++ nThings "turn" numGuessesAcrossAllGames ++ " in total"]
        , div [ css [sideTitleStyle] ] [ text "About" ]
        , p [] [ text "Game was created in Elm programming language. Watch how it was made here:"]
        , a [ href "https://youtu.be/aP9eKrnyxe4", Attr.target "_blank", css [linkStyle] ]
            [ text "https://youtu.be/aP9eKrnyxe4" ]
        ]


--------------- Computer prompts ---------------

-- Text pluralization
nCows = nThings "cow"
nBulls = nThings "bull"
nDigits = nThings "digit"

welcomePrompt : Html Msg
welcomePrompt =
    div []
    [ div [] [ text <| "I picked " ++ (toString digitsCount) ++ " different random digits" ]
    , div [] [ text <| "(for example: 2954, 0865, 3721)" ]
    , div [] [ text <| "Try to guess them!" ]
    ]

initialPrompt : Html Msg
initialPrompt =
    div []
    [ div [] [ text <| "I picked " ++ (toString digitsCount) ++ " different random digits" ]
    , div [] [ text <| "Try to guess them!" ]
    ]

firstGuessPrompt : List Int -> List Int -> Html Msg
firstGuessPrompt answer guess =
    let { cows, bulls } = countBullsCows answer guess
    in
        div []
        [ div [] [ text (digitsToString guess ++ "?")]
        , div [] [ text "Good guess!" ]
        , div [] [ text <| "My response is " ++ nCows cows ++ " and " ++ nBulls bulls ++ ", which means there " ++ pluralizeIs cows ]
        , ul [ css [listStyle] ]
            [ li [] [ text <| nDigits cows ++ " that you guessed correctly but in the wrong position and" ]
            , li [] [ text <| nDigits bulls  ++ " that you guessed correctly and in the correct position" ]
            ]
        , div [] [ text <| "Pick your next guess ..."]
        ]

secondGuessPrompt : List Int -> List Int -> Html Msg
secondGuessPrompt answer guess =
    let { cows, bulls } = countBullsCows answer guess
        correct = cows + bulls
    in
        div []
        [ div [] [ text (digitsToString guess ++ "? - Interesting")]
        , div [] [ text <| nCows cows ++ " and " ++ nBulls bulls ++ ", which as before means there " ++ pluralizeIs correct ++ " " ++ str cows ++ "+" ++ str bulls ++ "=" ++ str correct ++ " " ++ pluralize "digit" correct ++ " that you guessed correctly:" ]
        , ul [ css [listStyle] ]
            [ li [] [ text <| nDigits cows ++ " in the wrong position and" ]
            , li [] [ text <| nDigits bulls ++ " in the correct one"]
            ]
        , div [] [ text "Continue guessing ..." ]
        ]

thirdGuessPrompt : List Int -> List Int -> Html Msg
thirdGuessPrompt answer guess =
    let { cows, bulls } = countBullsCows answer guess
        --correct = cows + bulls
    in
        div []
        [ div [] [ text <| digitsToString guess ++ " - " ++ nCows cows ++ " and " ++ nBulls bulls ]
        , ul [ css [listStyle] ]
            [ li [] [ text <| nDigits cows ++ " in wrong position" ]
            , li [] [ text <| nDigits bulls ++ " in correct position" ]
            ]
        , div [] [ text "Keep guessing ..." ]
        ]

fourthGuessPrompt : List Int -> List Int -> Html Msg
fourthGuessPrompt answer guess =
    let { cows, bulls } = countBullsCows answer guess
        --correct = cows + bulls
    in
        div []
        [ div [] [ text <| digitsToString guess ++ " - " ++ nCows cows ++ " and " ++ nBulls bulls ]
        , div [] [ text "Continue guessing until you guess the correct number" ]
        ]

goodLuckPrompt : Html Msg
goodLuckPrompt =
    div []
    [ div [] [ text "Good luck!" ]
    , div [] [ text "Feel free to refer to the rules at the right side if needed" ]
    ]


closableOpenTime = 300
closableStyle = batch
    [ position absolute
    , overflow hidden
    , width (pct 100)
    --, debugBorder
    -- TODO: examine if the style on the following line is possible
    -- , borderTop3 (px 1) solid (rgb 0 0 0) , borderBottom3 (px 1) solid (rgb 0 0 0)
    , transition
      [ Tr.top closableOpenTime
      , Tr.bottom closableOpenTime
      , Tr.opacity closableOpenTime
      ]
    ]
closedBelow = batch 
    [ closableStyle
    , top (pct 100)
    , bottom zero
    , opacity zero
    ]
fullHeight = batch 
    [ closableStyle
    , top zero
    , bottom zero
    , opacity (num 1)
    ]
closedAbove = batch
    [ closableStyle
    , top zero
    , bottom (pct 100)
    , opacity zero
    ]

orderStyle order =
    case order of
        LT -> closedBelow
        EQ -> fullHeight
        GT -> closedAbove

propmtView : List Int -> List (List Int) -> Bool -> Html Msg
propmtView answer guesses completedTutorial =
    let
        numGuesses = List.length guesses
        nGuessesStyle n = orderStyle <| compare numGuesses n
        prompt n child =
            div [ css [nGuessesStyle n, centerChildren, secondaryTextStyle] ]
                [ child ]
    in
        div [ css 
                [ position absolute
                , top (pct 0)
                , width (pct 45)
                , height (pct 40)
                , pointerEvents none
                --, centerChildren
                , mainTextStyle
                --, opacity <| num <| if show then 1 else 0
                --, transition [ Tr.opacity 300 ]
                ]
            ] <|
            if not completedTutorial then
                [ prompt 0 <| welcomePrompt
                , prompt 1 <| firstGuessPrompt answer (guesses |> List.Extra.getAt 0 |> withDefault [])
                , prompt 2 <| secondGuessPrompt answer (guesses |> List.Extra.getAt 1 |> withDefault [])
                , prompt 3 <| thirdGuessPrompt answer (guesses |> List.Extra.getAt 2 |> withDefault [])
                , prompt 4 <| fourthGuessPrompt answer (guesses |> List.Extra.getAt 3 |> withDefault [])
                , prompt 5 <| goodLuckPrompt
                ]
            else
                [ prompt 0 <| initialPrompt
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
                  , GSS.everything
                    [ boxSizing borderBox
                    , cursor default
                    ]
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
                                , inputStyle
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
                    , div [ css [secondaryTextStyle] ]
                        (if gameOver then
                            [ p []
                              [ div [] [ text "Correct!" ]
                                , div [] [ text ("The number is " ++ answerText)]
                                , div [] [ text ("You guessed it in " ++ (str <| List.length model.guesses) ++ " turns") ]
                              ]
                              , button [ onClick Restart, css [ buttonStyle ] ] [ text "Play again" ]
                            ]
                         else []
                        )
                    -- , div [] [ text model.message ]
                    ]
                , rulesView
                , menuView model.gamesCompleted model.numGuessesAcrossAllGames
                , propmtView (model.answer |> withDefault []) model.guesses model.completedTutorial
                ] |> toUnstyled
            ]
        }
