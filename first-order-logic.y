%name generate
%tokentype { Token }
%error { parseError }

%token
	OR                 { TokenOR }
	AND                { TokenAND }
	NOT                { TokenNOT }
	BRACKET_OPEN       { TokenBracketOpen }
	BRACKET_CLOSE      { TokenBracketClose }
	PAREN_OPEN         { TokenParenOpen }
	PAREN_CLOSE        { TokenParenClose }
	TAUTOLOGY          { TokenTautology }
	CONTRADICTION      { TokenContradiction }
	FOR_ALL            { TokenForAll }
	THERE_EXISTS       { TokenThereExists }
	COLON              { TokenColon }
	COMMA              { TokenComma }
	IMPLIES            { TokenImplies }
	FREE_VARIABLE      { TokenFreeVariable }
	PREDICATE          { TokenPredicate }
	NEWLINE            { TokenNewline }

%%

program
	:                                       { [] }
	| formulaList optNEWLINE                { $1 }

formulaList
	: formula                               { [(Formula $1)] }
	| formulaList NEWLINE formula           { $1 ++ [(Formula $3)] }

formula
	: expr                                      { $1 }
	| expr IMPLIES expr                         { Implication $1 $3 }
	| FOR_ALL argList quantifierBody            { UniversalQuantifier $2 $3 }
	| THERE_EXISTS argList quantifierBody       { ExistentialQuantifier False $2 $3 }

quantifierBody: optCOLON formula                { Formula $2 }

expr: exprOR { $1 }

exprOR
	: exprAND                               { $1 }
	| exprOR OR exprAND                     { Or $1 $3 }

exprAND
	: exprValue                             { $1 }
	| exprAND AND exprValue                 { And $1 $3 }

exprValue
	: atomic                                { $1 }
	| PAREN_OPEN formula PAREN_CLOSE        { Formula $2 }
	| BRACKET_OPEN formula BRACKET_CLOSE    { Formula $2 }
	| TAUTOLOGY                             { Tautology $1 }
	| CONTRADICTION                         { Contradiction $1 }
	| NOT exprValue                         { Not $2 }

atomic: PREDICATE index                     { Atomic $1 $2 }

index
	: PAREN_OPEN argList PAREN_CLOSE        { $2 }
	| BRACKET_OPEN argList BRACKET_CLOSE    { $2 }

argList
	: arg                                   { [(Arg $1)] }
	| argList COMMA arg                     { $1 ++ [(Arg $3)] }

arg: FREE_VARIABLE                          { $1 }

optCOLON:   { False } | COLON   { $1 }
optNEWLINE: { False } | NEWLINE { $1 }

{
eval = do
	s <- readFile "./simple-grammar-sample.fol"
	putStr s
	print (alexScanTokens s)

type FormulaList = [Formula]

data Formula
	= Formula Formula
	| Or Formula Formula
	| And Formula Formula
	| Not Formula
	| Implication Formula Formula
	| UniversalQuantifier ArgList Formula
	| ExistentialQuantifier Bool ArgList Formula
	| Tautology Token
	| Contradiction Token
	| Atomic String
	deriving Show

data Arg
	= Arg FreeVariable
	deriving Show

data FreeVariable
	= FreeVariable Token
	deriving Show

type ArgList = [Arg]

data Token
	= TokenOR
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
	| TokenFreeVariable
	| TokenPredicate
	| TokenNewline
	deriving Show

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
