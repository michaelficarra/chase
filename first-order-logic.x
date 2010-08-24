%wrapper "basic"

tokens :-

	[\r\n]+                         { \s -> ("NEWLINE",s) }
	[\ \t]+                         ; -- ignore whitespace
	"-- "[^\r\n]*                   ; -- ignore comments
	"#"[^\r\n]*                     ; -- ignore comments
	"/*"([^\*]|"*"[^\/])*"*/"       ; -- ignore comments
	"Exists"                        { \s -> ("THERE_EXISTS",s) }
	"For"[Aa]"ll"                   { \s -> ("FOR_ALL",s) }
	[Tt]"autology"                  { \s -> ("TAUTOLOGY",s) }
	[Cc]"ontradiction"              { \s -> ("CONTRADICTION",s) }
	"->"                            { \s -> ("IMPLIES",s) }
	"!"                             { \s -> ("NOT",s) }
	"|"                             { \s -> ("OR",s) }
	"&"                             { \s -> ("AND",s) }
	"("                             { \s -> ("PAREN_OPEN",s) }
	")"                             { \s -> ("PAREN_CLOSE",s) }
	"["                             { \s -> ("BRACKET_OPEN",s) }
	"]"                             { \s -> ("BRACKET_CLOSE",s) }
	":"                             { \s -> ("COLON",s) }
	","                             { \s -> ("COMMA",s) }
	[a-z][a-z0-9_']*                { \s -> ("FREE_VARIABLE",s) }
	[A-Z][a-z0-9_']*                { \s -> ("PREDICATE",s) }

{
printTokens = do
	s <- readFile "./simple-grammar-sample.fol"
	putStr s
	print (alexScanTokens s)
}
