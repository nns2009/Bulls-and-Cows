module Main exposing (..)

import List.Extra
import Maybe exposing (withDefault)
import Random

import Browser

import Css exposing (..)
-- import Html
import Html.Styled as H
import Html.Styled exposing (node, text, div, th, tr, td, input, button, Html, toUnstyled)
import Html.Styled.Attributes as Attr
import Html.Styled.Attributes exposing (value, type_, css, href, src)
import Html.Styled.Events exposing (onClick, onInput, onSubmit)
import Debug exposing (toString)
import Html.Styled exposing (form)

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

mainFontFamily = "Calibri"
mainFontSize = em 1.6

mainText = batch
    [ fontSize mainFontSize
    , fontFamilies [ mainFontFamily ]
    ]

cellStyle = batch
    [ width (pct 33)
    , border3 (px 1) solid (rgb 150 150 150)
    , textAlign center
    ]

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
    in
        { title = answerText
        , body =
            [ div [css [displayFlex, alignItems center, justifyContent center, height (pct 100)]]
                [ node "style" [] [ text """
                    html { height: 100%; }
                    body { height: 100%; }
                    """ ]
                , div [css
                    [ width (px 250)
                    , border3 (px 1) solid (rgb 255 0 0)
                    , displayFlex
                    , flexDirection column
                    ]]
                    [ H.table [ css
                        [ mainText
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
                            , mainText
                            ]
                        ] []
                        , button
                            [ Attr.disabled ((String.length model.guess /= digitsCount) || ((model.guess |> String.toList |> List.Extra.unique |> List.length) /= digitsCount))
                            , css
                            [ mainText
                            , width (em 1.6)
                            ]
                            ] [ text "?" ] ]
                        
                    , div []
                        [ text model.message ]
                    ]
                ] |> toUnstyled
            ]
        }
