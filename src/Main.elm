module Main exposing (..)

import List.Extra
import Maybe exposing (withDefault)
import Random

import Browser

import Css exposing (..)
import Css.Transitions as Tr
import Css.Transitions exposing (transition)
import Css.Global as GSS exposing (children)
-- import Html
import Html.Styled as H
import Html.Styled exposing (text, div, span, p, th, tr, td, ul, li, input, button, form, Html, toUnstyled)
import Html.Styled.Attributes as Attr
import Html.Styled.Attributes exposing (id, class, value, type_, css)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Debug exposing (toString)

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
init flags =
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

fillAllSpace = batch
    [ position absolute
    , top zero
    , bottom zero
    , left zero
    , right zero
    ]

centerChildren = batch
    [ displayFlex
    , alignItems center
    , justifyContent center
    ]

mainFontFamily = "Calibri"
mainFontSize = em 1.6

mainTextStyle = batch
    [ fontSize mainFontSize
    , fontFamilies [ mainFontFamily ]
    ]

listStyle = batch
    [ paddingLeft (em 1) ]

cellStyle = batch
    [ width (pct 33)
    , border3 (px 1) solid (rgb 150 150 150)
    , textAlign center
    ]

rulesPanelOpenTime = 350
rulesClosedWidth = px 100
rulesOpenWidth = px 290
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
    tr [ ]
      [ td [ css [cellStyle] ] [text guessString]
      , td [ css [cellStyle] ] [text <| String.fromInt cows]
      , td [ css [cellStyle] ] [text <| String.fromInt bulls]
      ]
      --  [ text (guessString ++ " - " ++ (String.fromInt cows) ++ " cows, " ++ (String.fromInt bulls) ++ " bulls")]

view : Model -> Browser.Document Msg
view model =
    let answerText =
          case model.answer of
            Nothing ->
                "No digits generated so far"
            Just digits ->
                String.join "" (List.map toString digits)
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
                  ]
                , div [css
                    [ width (px 250)
                    , border3 (px 1) solid (rgb 255 0 0)
                    , displayFlex
                    , flexDirection column
                    ]]
                    [ H.table [ css
                        [ mainTextStyle
                        , borderCollapse collapse
                        ]]
                        <| tr []
                          [ th [ css [cellStyle] ] [ text "-" ]
                          , th [ css [cellStyle] ] [ text "Cows" ]
                          , th [ css [cellStyle] ] [ text "Bulls" ]
                          ]
                        :: (model.guesses |> List.map (guessView (model.answer |> withDefault [])))
                    , form [onSubmit GuessCheck, css [displayFlex]]
                        [ input
                        [ value model.guess
                        , onInput GuessChange
                        --, 
                        , css
                            [ flex2 (num 1) (num 1)
                            , minWidth (px 0)
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
                            , width (em 1.6)
                            ]
                            ] [ text "?" ] ]
                    , div [ css [mainTextStyle] ]
                        (if gameOver then
                            [ div [] [ text "Correct!" ]
                            , div [] [ text ("You guessed the number in " ++ (String.fromInt <| List.length model.guesses) ++ " guesses") ]
                            , button [ onClick Restart, css [ mainTextStyle ] ] [ text "Play again"]
                            ]
                         else []
                        )
                    , div []
                        [ text model.message ]
                    ]
                , div [ css
                  [ position absolute
                  , overflow hidden
                  , right zero
                  , top zero
                  , bottom zero
                  , backgroundColor debugBackgroundColor
                  , rulesTextStyle
                  , transition
                    [ Tr.width rulesPanelOpenTime
                    ]
                  , width rulesClosedWidth
                  , children
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
                    ]
                  , hover
                    [ width rulesOpenWidth
                    , children
                      [ GSS.class "revealHeader"
                          [ opacity (num 0)
                          , visibility hidden
                          ]
                      , GSS.class "revealContent"
                          [ opacity (num 1)
                          ]
                      ]
                    ]
                  ]]
                  [ div [ class "revealHeader", css [fillAllSpace] ] [ text "Rules" ]
                  , div [ class "revealContent", css [height (pct 100)] ]
                    [ div []
                        [ p [] [ text "Computer picked up a random number (with a specified number of digits) where all digits are different - your goal is to guess it" ]
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
                ] |> toUnstyled
            ]
        }
