let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220510/packages.dhall sha256:0b0d4db1f2f0acd3b37fa53220644ac6f64cf9b5d0226fd097c0593df563d5be

in  upstream
  with spec-mocha =
    { dependencies =
      [ "aff"
      , "console"
      , "datetime"
      , "effect"
      , "either"
      , "foldable-traversable"
      , "maybe"
      , "newtype"
      , "prelude"
      , "spec"
      , "profunctor"
      , "profunctor-lenses"
      , "unsafe-coerce"
      , "ordered-collections"
      , "refs"
      ]
    , repo = "https://github.com/restaumatic/purescript-spec-mocha.git"
    , version = "v4.0.0-restaumatic2"
    }
