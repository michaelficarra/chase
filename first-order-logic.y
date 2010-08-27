{
module Main where
import Lexer
}
%name generate
%tokentype { Token }
%error { parseError }

%token
	OR                 { TokenOR }
	AND                { TokenAND }
	NOT                { TokenNOT }
	"["                { TokenBracketOpen }
	"]"                { TokenBracketClose }
	"("                { TokenParenOpen }
	")"                { TokenParenClose }
	TAUTOLOGY          { TokenTautology }
	CONTRADICTION      { TokenContradiction }
	FOR_ALL            { TokenForAll }
	THERE_EXISTS       { TokenThereExists }
	":"                { TokenColon }
	","                { TokenComma }
	"->"               { TokenImplies }
	FREE_VARIABLE      { TokenFreeVariable $$ }
	PREDICATE          { TokenPredicate $$ }
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
	| expr "->" expr                            { Implication $1 $3 }
	| FOR_ALL argList quantifierBody            { UniversalQuantifier $2 $3 }
	| THERE_EXISTS argList quantifierBody       { ExistentialQuantifier $2 $3 }

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
	| "(" formula ")"                       { Formula $2 }
	| "[" formula "]"                       { Formula $2 }
	| TAUTOLOGY                             { Tautology $1 }
	| CONTRADICTION                         { Contradiction $1 }
	| NOT exprValue                         { Not $2 }

atomic: PREDICATE index                     { Atomic $1 $2 }

index
	: "(" argList ")"                       { $2 }
	| "[" argList "]"                       { $2 }

argList
	: arg                                   { [(Arg $1)] }
	| argList "," arg                       { $1 ++ [(Arg $3)] }

arg: FREE_VARIABLE                          { FreeVariable $1 }

optCOLON:   { Nil } | ":"     { $1 }
optNEWLINE: { Nil } | NEWLINE { $1 }

{
main = do
	s <- getContents -- readFile "./simple-grammar-sample.fol"
	putStr s
	let parseTree = generate (alexScanTokens s)
	putStrLn ("parseTree: " ++ show(parseTree))

data Formula
	= Formula Formula
	| Or Formula Formula
	| And Formula Formula
	| Not Formula
	| Implication Formula Formula
	| UniversalQuantifier ArgList Formula
	| ExistentialQuantifier ArgList Formula
	| Tautology Token
	| Contradiction Token
	| Atomic String ArgList
	deriving (Show, Eq)

data Arg
	= Arg FreeVariable
	deriving (Show, Eq)

data FreeVariable
	= FreeVariable String
	deriving (Show, Eq)

type ArgList = [Arg]

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
