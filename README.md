*warning: this is NOT ready for public use*

Build and test ruby parser:

	rake ruby

The output should be a string that is functionally equivalent to the input.

Build and test haskell parser:

	rake haskell

When in ghci, type `printTokens` to list the tokens that the lexer found. `printParseTree` will print the parse tree when that feature is complete.