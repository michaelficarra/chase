{module Lexer where}

%wrapper "posn"

tokens :-

	[\ \t]+                         ; -- ignore whitespace
	"-- "[^\r\n]*                   ; -- ignore comments
	"#"[^\r\n]*                     ; -- ignore comments
	"/*"([^\*]|"*"[^\/])*"*/"       ; -- ignore comments
	[\r\n]+                         { \p s -> TokenNewline p }
	"Exists"                        { \p s -> TokenExists p }
	"For"[Aa]"ll"                   { \p s -> TokenForAll p }
	[Tt]"autology"                  { \p s -> TokenTautology p }
	[Cc]"ontradiction"              { \p s -> TokenContradiction p }
	"->"                            { \p s -> TokenImplies p }
	"!"                             { \p s -> TokenNOT p }
	"="                             { \p s -> TokenEQ p }
	"|"                             { \p s -> TokenOR p }
	"&"                             { \p s -> TokenAND p }
	"("                             { \p s -> TokenParenOpen p }
	")"                             { \p s -> TokenParenClose p }
	"["                             { \p s -> TokenBracketOpen p }
	"]"                             { \p s -> TokenBracketClose p }
	":"                             { \p s -> TokenColon p }
	","                             { \p s -> TokenComma p }
	[a-z][A-Za-z0-9_']*             { TokenVariable }
	[A-Z][A-Za-z0-9_']*             { TokenPredicate }

{

data Token
	= Nil
	| TokenOR AlexPosn
	| TokenAND AlexPosn
	| TokenNOT AlexPosn
	| TokenEQ AlexPosn
	| TokenBracketOpen AlexPosn
	| TokenBracketClose AlexPosn
	| TokenParenOpen AlexPosn
	| TokenParenClose AlexPosn
	| TokenTautology AlexPosn
	| TokenContradiction AlexPosn
	| TokenForAll AlexPosn
	| TokenExists AlexPosn
	| TokenColon AlexPosn
	| TokenComma AlexPosn
	| TokenImplies AlexPosn
	| TokenVariable AlexPosn String
	| TokenPredicate AlexPosn String
	| TokenNewline AlexPosn
	deriving (Eq,Show)

tokenPosn :: Token -> AlexPosn
tokenPosn (TokenOR p) = p
tokenPosn (TokenAND p) = p
tokenPosn (TokenNOT p) = p
tokenPosn (TokenEQ p) = p
tokenPosn (TokenBracketOpen p) = p
tokenPosn (TokenBracketClose p) = p
tokenPosn (TokenParenOpen p) = p
tokenPosn (TokenParenClose p) = p
tokenPosn (TokenTautology p) = p
tokenPosn (TokenContradiction p) = p
tokenPosn (TokenForAll p) = p
tokenPosn (TokenExists p) = p
tokenPosn (TokenColon p) = p
tokenPosn (TokenComma p) = p
tokenPosn (TokenImplies p) = p
tokenPosn (TokenVariable p _) = p
tokenPosn (TokenPredicate p _) = p
tokenPosn (TokenNewline p) = p

showToken :: Token -> String
showToken (TokenOR p) = "OR"
showToken (TokenAND p) = "AND"
showToken (TokenNOT p) = "NOT"
showToken (TokenEQ p) = "EQ"
showToken (TokenBracketOpen p) = "BracketOpen"
showToken (TokenBracketClose p) = "BracketClose"
showToken (TokenParenOpen p) = "ParenOpen"
showToken (TokenParenClose p) = "ParenClose"
showToken (TokenTautology p) = "Tautology"
showToken (TokenContradiction p) = "Contradiction"
showToken (TokenForAll p) = "ForAll"
showToken (TokenExists p) = "Exists"
showToken (TokenColon p) = "Colon"
showToken (TokenComma p) = "Comma"
showToken (TokenImplies p) = "Implies"
showToken (TokenVariable p s) = "Variable"
showToken (TokenPredicate p s) = "Predicate"
showToken (TokenNewline p) = "Newline"

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn offset line column) = line

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn offset line column) = column

scanTokens str = go (alexStartPos,'\n',str)
	where go inp@(pos,_,str) =
		case alexScan inp 0 of
			AlexEOF -> []
			AlexError _ -> error ("lexical error: line " ++ show (getLineNum(pos)) ++ ", column " ++ show (getColumnNum(pos)))
			AlexSkip  inp' len     -> go inp'
			AlexToken inp' len act -> act pos (take len str) : go inp'

}
