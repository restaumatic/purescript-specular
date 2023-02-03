{ name = "specular"
, dependencies =
  [ "aff"
  , "arrays"
  , "avar"
  , "console"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "integers"
  , "invariant"
  , "maybe"
  , "newtype"
  , "partial"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "random"
  , "record"
  , "refs"
  , "spec"
  , "spec-mocha"
  , "strings"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "unfoldable"
  , "unsafe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/restaumatic/purescript-specular"
}
