{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "affjax"
    , "console"
    , "control"
    , "css"
    , "effect"
    , "halogen"
    , "halogen-css"
    , "parsing"
    , "psci-support"
    , "random"
    , "tuples"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
