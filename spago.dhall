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
  , "css"
  , "dom-filereader"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "lists"
  , "maybe"
  , "partial"
  , "prelude"
  , "web-events"
  , "web-file"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
