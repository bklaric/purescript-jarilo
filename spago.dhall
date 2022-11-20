{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "jarilo"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
  , "contravariant"
  , "control"
  , "effect"
  , "either"
  , "error"
  , "foldable-traversable"
  , "foreign"
  , "http-methods"
  , "integers"
  , "lists"
  , "maybe"
  , "newtype"
  , "options"
  , "ordered-collections"
  , "parallel"
  , "prelude"
  , "quickcheck"
  , "record"
  , "refs"
  , "strings"
  , "transformers"
  , "tuples"
  , "undefined"
  , "unfoldable"
  , "unsafe-coerce"
  , "uri"
  , "variant"
  , "yoga-json"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
