{ module Lexer where }
%wrapper "basic"

tokens :-

	[\r\n]+                         { \s -> TokenNewline }
	[\ \t]+                         ; -- ignore whitespace
	"-- "[^\r\n]*                   ; -- ignore comments
	"#"[^\r\n]*                     ; -- ignore comments
	"/*"([^\*]|"*"[^\/])*"*/"       ; -- ignore comments
	"Exists"                        { \s -> TokenThereExists }
	"For"[Aa]"ll"                   { \s -> TokenForAll }
	[Tt]"autology"                  { \s -> TokenTautology }
	[Cc]"ontradiction"              { \s -> TokenContradiction }
	"->"                            { \s -> TokenImplies }
	"!"                             { \s -> TokenNOT }
	"|"                             { \s -> TokenOR }
	"&"                             { \s -> TokenAND }
	"("                             { \s -> TokenParenOpen }
	")"                             { \s -> TokenParenClose }
	"["                             { \s -> TokenBracketOpen }
	"]"                             { \s -> TokenBracketClose }
	":"                             { \s -> TokenColon }
	","                             { \s -> TokenComma }
	[a-z][a-z0-9_']*                { \s -> TokenFreeVariable s }
	[A-Z][a-z0-9_']*                { \s -> TokenPredicate s }

{
data Token
	= Nil
	| TokenOR
	| TokenAND
	| TokenNOT
	| TokenBracketOpen
	| TokenBracketClose
	| TokenParenOpen
	| TokenParenClose
	| TokenTautology
	| TokenContradiction
	| TokenForAll
	| TokenThereExists
	| TokenColon
	| TokenComma
	| TokenImplies
	| TokenFreeVariable String
	| TokenPredicate String
	| TokenNewline
	deriving (Eq,Show)

printTokens = do
	s <- readFile "./simple-grammar-sample.fol"
	putStr s
	print (alexScanTokens s)
}
