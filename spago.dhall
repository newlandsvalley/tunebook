{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-abc-editor"
, dependencies =
  [ "abc-parser"
  , "abc-scores"
  , "aff"
  , "arrays"
  , "console"
  , "css"
  , "dom-filereader"
  , "effect"
  , "either"
  , "ensemble-scores"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "web-events"
  , "web-file"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
