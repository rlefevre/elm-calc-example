# elm-calc-example

A basic Elm 0.18 arithmetic expressions parser/evaluator example to get started with [elm-tools/parser](http://package.elm-lang.org/packages/elm-tools/parser/latest).

It also uses [rtfeldman/elm-css](http://package.elm-lang.org/packages/rtfeldman/elm-css/latest) to style the page.

## Usage

You can play with it [here](https://rlefevre.github.io/elm-calc-example/).

Or in Ellie:

https://ellie-app.com/bvq3hRHcpa1/1

Or locally for example with `elm-reactor`:

> npm install -g elm

> elm reactor

## Notes

* If you test some recursive decoders or parsers with Elm 0.18, be careful with [this bug](https://github.com/elm-lang/elm-compiler/issues/1591).
See [Help, my recursive Decoder caused a runtime exception!](https://blog.ilias.xyz/help-my-recursive-decoder-caused-a-runtime-exception-453d46a99e1e) for more information.
* For Elm 0.19, see [this official parser example instead](https://github.com/elm/parser/blob/1.0.0/examples/Math.elm).

