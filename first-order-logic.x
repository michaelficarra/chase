{
	module Main (main) where
}

%wrapper "basic"

tokens :-

	\n+                         { \s -> ("NEWLINE",s) }
	[\ \t]+                     ; -- ignore whitespace
	"--".*                      ; -- ignore comments
	"facts"                     { \s -> ("FACTS",s) }
	"rules"                     { \s -> ("RULES",s) }
	"chase"                     { \s -> ("CHASE",s) }
	[A-Z]+[a-z]+                { \s -> ("PREDICATE",s) }
	"\x22A4"                    { \s -> ("TAUTOLOGY",s) }
	"\x22A5"                    { \s -> ("CONTRADICTION",s) }

	"\d"+                       { \s -> ("INTEGER",s) }
	"\x2200"[a-z]+              { \s -> ("FOR_ALL", drop 0 s) }
	[a-z][a-zA-Z0-9_\-]*        { \s -> ("IDENTIFIER",s) }
	"{"[\ \t]*"}"|"\x2205"      { \s -> ("EMPTY_SET",s) }

	"("                         { \s -> ("PAREN_OPEN",s) }
	")"                         { \s -> ("PAREN_CLOSE",s) }
	"{"                         { \s -> ("BRACE_OPEN",s) }
	"}"                         { \s -> ("BRACE_CLOSE",s) }
	":"                         { \s -> ("COLON",s) }

{
main = do
	s <- readFile "./simple-grammar-sample.fol"
	putStr s
	print (alexScanTokens s)
}
