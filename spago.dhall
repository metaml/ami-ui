{ name = "ami-u"
, dependencies = [ "aff"
                 , "arrays"
                 , "argonaut"
    	       	 , "console"
                 , "const"
                 , "effect"
                 , "either"
                 , "foreign-object"
                 , "halogen"
                 , "halogen-formless"
                 , "halogen-storybook"
                 , "maybe"
                 , "prelude"
                 , "tuples"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
