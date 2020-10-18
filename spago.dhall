{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "jarilo"
, dependencies = [ "console", "effect", "http-methods", "uri", "variant", "record", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
