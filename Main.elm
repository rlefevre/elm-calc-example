module Main exposing (main)

import Css
import Html.Styled exposing (Attribute, Html, br, pre, h1, div, input, li, p, program, span, styled, text, ul)
import Html.Styled.Attributes exposing (css, placeholder, value)
import Html.Styled.Events exposing (onInput)
import Parser exposing (..)


type alias Model =
    String


type Msg
    = SetExpr String



-- PARSER --


type Expression
    = Number Float
    | Neg Expression
    | Add Expression Expression
    | Sub Expression Expression
    | Mul Expression Expression
    | Div Expression Expression


init : ( Model, Cmd Msg )
init =
    ( "", Cmd.none )


expression : Parser Expression
expression =
    inContext "expression" <|
        lazy (\_ -> chainLeft addOp term)


chainLeft : Parser ( String, a -> a -> a ) -> Parser a -> Parser a
chainLeft op p =
    let
        accumulate x =
            oneOf
                [ op
                    |> andThen
                        (\( context, f ) ->
                            inContext context
                                (p |> andThen (\y -> accumulate (f x y)))
                        )
                , succeed x
                ]
    in
        p |> andThen accumulate


addOp : Parser ( String, Expression -> Expression -> Expression )
addOp =
    oneOf
        [ succeed ( "addition", Add )
            |. symbol "+"
        , succeed ( "substraction", Sub )
            |. symbol "-"
        ]


spaces : Parser ()
spaces =
    ignore zeroOrMore (\c -> c == ' ')


mulOp : Parser ( String, Expression -> Expression -> Expression )
mulOp =
    oneOf
        [ succeed ( "multiplication", Mul )
            |. symbol "*"
        , succeed ( "division", Div )
            |. symbol "/"
        ]


term : Parser Expression
term =
    lazy (\_ -> chainLeft mulOp factor)


factor : Parser Expression
factor =
    succeed identity
        |. spaces
        |= oneOf
            [ succeed Number
                |= float
            , succeed Neg
                |. symbol "-"
                |= lazy (\_ -> factor)
            , lazy (\_ -> parens expression)
            ]
        |. spaces


parens : Parser a -> Parser a
parens p =
    succeed identity
        |. symbol "("
        |= lazy (\_ -> p)
        |. symbol ")"


parse : Parser Expression
parse =
    succeed identity
        |= expression
        |. end


eval : Expression -> Float
eval expr =
    case expr of
        Number x ->
            x

        Neg x ->
            negate (eval x)

        Add left right ->
            eval left + eval right

        Sub left right ->
            eval left - eval right

        Mul left right ->
            eval left * eval right

        Div left right ->
            eval left / eval right


calc : String -> Result Error ( Expression, Float )
calc expr =
    case run parse expr of
        Ok expr ->
            Ok ( expr, eval expr )

        Err error ->
            Err error



-- VIEW --


page : List (Attribute msg) -> List (Html msg) -> Html msg
page =
    styled div
        [ Css.padding (Css.pct 2)
        , Css.margin (Css.pct 2)
        , Css.displayFlex
        , Css.flexDirection Css.column
        , Css.alignItems Css.stretch
        ]


heading : List (Attribute msg) -> List (Html msg) -> Html msg
heading =
    styled h1
        [ Css.fontSize (Css.rem 1)
        , Css.color (Css.hex "#2196F3")
        , Css.marginTop (Css.rem 2)
        ]


codeSpan : List (Attribute msg) -> List (Html msg) -> Html msg
codeSpan =
    styled span
        [ Css.fontFamily Css.monospace
        , Css.fontSize (Css.rem 1)
        ]


errorCodeSpan : List (Attribute msg) -> List (Html msg) -> Html msg
errorCodeSpan =
    styled codeSpan
        [ Css.color (Css.hex "#F44336") ]


symbolSpan : List (Attribute msg) -> List (Html msg) -> Html msg
symbolSpan =
    styled codeSpan
        [ Css.backgroundColor (Css.hsl 0 0 0.92) ]


view : Model -> Html Msg
view model =
    page []
        [ heading []
            [ text "Input" ]
        , input
            [ value model
            , onInput SetExpr
            , placeholder "Expression using numbers, +, -, *, / or parentheses"
            ]
            []
        , viewIf (not <| String.isEmpty <| String.trim model) <|
            case calc model of
                Ok ( expr, result ) ->
                    viewResult expr result

                Err error ->
                    viewError error
        ]


viewIf : Bool -> Html msg -> Html msg
viewIf condition content =
    if condition then
        content
    else
        text ""


viewResult : Expression -> Float -> Html Msg
viewResult expr result =
    div []
        [ heading []
            [ text "Result" ]
        , codeSpan []
            [ text (" = " ++ toString result) ]
        , heading []
            [ text "Expression" ]
        , codeSpan []
            [ text (toString expr) ]
        ]


viewError : Error -> Html Msg
viewError error =
    div []
        [ heading []
            [ text "Error" ]
        , viewContextProblem error
        , heading []
            [ text "Raw Error" ]
        , p []
            [ codeSpan []
                [ text (toString error) ]
            ]
        ]


viewContextProblem : Error -> Html Msg
viewContextProblem error =
    div []
        [ p []
            [ text ("There is a problem with " ++ getErrorContext error ++ ":") ]
        , p
            [ css [ Css.paddingLeft (Css.rem 1) ] ]
            [ pre []
                [ codeSpan []
                    [ text (getPreviousContextSource error) ]
                , errorCodeSpan []
                    [ text (getErrorSource error) ]
                ]
            , pre []
                [ errorCodeSpan []
                    [ String.length (getPreviousContextSource error)
                        |> (flip String.repeat) " "
                        |> text
                    , text "^"
                    ]
                ]
            ]
        , p []
            [ text "Looking for "
            , viewExpected error.problem
            ]
        ]


getErrorContext : Error -> String
getErrorContext error =
    case error.context of
        lastContext :: _ ->
            "this " ++ lastContext.description

        _ ->
            "this expression"


getPreviousContextSource : Error -> String
getPreviousContextSource error =
    case error.context of
        lastContext :: contextBefore :: _ ->
            String.slice
                (contextBefore.col - 1)
                (error.col - 1)
                error.source

        _ ->
            String.left (error.col - 1) error.source


getErrorSource : Error -> String
getErrorSource error =
    String.dropLeft (error.col - 1) error.source


viewExpected : Problem -> Html Msg
viewExpected problem =
    case problem of
        BadOneOf problems ->
            List.map viewExpected problems
                |> List.map (\text -> li [] [ text ])
                |> ul []

        BadFloat ->
            text "a valid number"

        ExpectingEnd ->
            text "end of input"

        ExpectingSymbol symbol ->
            span []
                [ text "the symbol "
                , symbolSpan [] [ text symbol ]
                ]

        _ ->
            text ""



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update (SetExpr expr) model =
    ( expr, Cmd.none )



-- MAIN --


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
