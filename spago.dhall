{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}

{ name = "my-project"
, dependencies = [ "aff"
    	       	 , "console"
                 , "argonaut"
                 , "arrays"
                 , "dom-indexed"
                 , "effect"
                 , "either"
                 , "foldable-traversable"
                 , "foreign-object"
                 , "halogen"
                 , "halogen-formless"
                 , "halogen-storybook"
                 , "maybe"
                 , "media-types"
                 , "newtype"
                 , "prelude"
                 , "strings"
                 , "transformers"
                 , "tuples"
                 , "web-dom"
                 , "web-file"
                 , "web-html"
                 , "web-storage"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
