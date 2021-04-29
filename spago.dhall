{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "affjax"
  , "arrays"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "maybe"
  , "node-fs"
  , "psci-support"
  , "random"
  , "stringutils"
  , "transformers"
  , "unfoldable"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
