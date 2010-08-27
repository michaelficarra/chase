{ module Lexer where }
%wrapper "posn"

tokens :-

	[\r\n]+                         { \p s -> TokenNewline p }
	[\ \t]+                         ; -- ignore whitespace
	"-- "[^\r\n]*                   ; -- ignore comments
	"#"[^\r\n]*                     ; -- ignore comments
	"/*"([^\*]|"*"[^\/])*"*/"       ; -- ignore comments
	"Exists"                        { \p s -> TokenThereExists p }
	"For"[Aa]"ll"                   { \p s -> TokenForAll p }
	[Tt]"autology"                  { \p s -> TokenTautology p }
	[Cc]"ontradiction"              { \p s -> TokenContradiction p }
	"->"                            { \p s -> TokenImplies p }
	"!"                             { \p s -> TokenNOT p }
	"|"                             { \p s -> TokenOR p }
	"&"                             { \p s -> TokenAND p }
	"("                             { \p s -> TokenParenOpen p }
	")"                             { \p s -> TokenParenClose p }
	"["                             { \p s -> TokenBracketOpen p }
	"]"                             { \p s -> TokenBracketClose p }
	":"                             { \p s -> TokenColon p }
	","                             { \p s -> TokenComma p }
	[a-z][a-z0-9_']*                { \p s -> TokenFreeVariable p s }
	[A-Z][a-z0-9_']*                { \p s -> TokenPredicate p s }

{
data Token
	= Nil
	| TokenOR AlexPosn
	| TokenAND AlexPosn
	| TokenNOT AlexPosn
	| TokenBracketOpen AlexPosn
	| TokenBracketClose AlexPosn
	| TokenParenOpen AlexPosn
	| TokenParenClose AlexPosn
	| TokenTautology AlexPosn
	| TokenContradiction AlexPosn
	| TokenForAll AlexPosn
	| TokenThereExists AlexPosn
	| TokenColon AlexPosn
	| TokenComma AlexPosn
	| TokenImplies AlexPosn
	| TokenFreeVariable AlexPosn String
	| TokenPredicate AlexPosn String
	| TokenNewline AlexPosn
	deriving (Eq,Show)

tokenPosn (TokenOR p) = p
tokenPosn (TokenAND p) = p
tokenPosn (TokenNOT p) = p
tokenPosn (TokenBracketOpen p) = p
tokenPosn (TokenBracketClose p) = p
tokenPosn (TokenParenOpen p) = p
tokenPosn (TokenParenClose p) = p
tokenPosn (TokenTautology p) = p
tokenPosn (TokenContradiction p) = p
tokenPosn (TokenForAll p) = p
tokenPosn (TokenThereExists p) = p
tokenPosn (TokenColon p) = p
tokenPosn (TokenComma p) = p
tokenPosn (TokenImplies p) = p
tokenPosn (TokenFreeVariable p _) = p
tokenPosn (TokenPredicate p _) = p
tokenPosn (TokenNewline p) = p

showToken (TokenOR p) = "TokenOR"
showToken (TokenAND p) = "TokenAND"
showToken (TokenNOT p) = "TokenNOT"
showToken (TokenBracketOpen p) = "TokenBracketOpen"
showToken (TokenBracketClose p) = "TokenBracketClose"
showToken (TokenParenOpen p) = "TokenParenOpen"
showToken (TokenParenClose p) = "TokenParenClose"
showToken (TokenTautology p) = "TokenTautology"
showToken (TokenContradiction p) = "TokenContradiction"
showToken (TokenForAll p) = "TokenForAll"
showToken (TokenThereExists p) = "TokenThereExists"
showToken (TokenColon p) = "TokenColon"
showToken (TokenComma p) = "TokenComma"
showToken (TokenImplies p) = "TokenImplies"
showToken (TokenFreeVariable p s) = "TokenFreeVariable"
showToken (TokenPredicate p s) = "TokenPredicate"
showToken (TokenNewline p) = "TokenNewline"


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
