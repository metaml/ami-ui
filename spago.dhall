{ name = "ami-u"
, dependencies = [ "aff"
                 , "arrays"
                 , "argonaut-codecs"
                 , "argonaut-core"
    	       	 , "console"
                 , "const"
                 , "effect"
                 , "either"
                 , "fetch"
                 , "foreign-object"
                 , "foreign"
                 , "halogen"
                 , "halogen-formless"
                 , "halogen-storybook"
                 , "maybe"
                 , "prelude"
                 , "random"
                 , "tuples"
                 , "undefined"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
