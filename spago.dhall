{ name = "ami-u"
, dependencies = [ "aff"
    	       	 , "console"
                 , "effect"
                 , "either"
                 , "foldable-traversable"
                 , "foreign-object"
                 , "halogen"
                 , "halogen-formless"
                 , "halogen-storybook"
                 , "maybe"
                 , "prelude"
                 , "tuples"
                 , "web-dom"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
