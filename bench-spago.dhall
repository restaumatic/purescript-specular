{ name = "${(./spago.dhall).name}-bench"
, dependencies = (./spago.dhall).dependencies #
  [ "lists"
  ]
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # [ "bench/**/*.purs" ]
, license = (./spago.dhall).license
, repository = (./spago.dhall).repository
}
