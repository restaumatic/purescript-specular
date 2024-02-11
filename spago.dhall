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
  , "random"
  , "record"
  , "refs"
  , "safe-coerce"
  , "spec"
  , "spec-mocha"
  , "strings"
  , "transformers"
  , "tuples"
  , "type-equality"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "unsafe-reference"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "bench/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/restaumatic/purescript-specular"
}
