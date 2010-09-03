{
module Main where
import Lexer
import List
}

%name generate
%tokentype { Token }
%error { parseError }

%token
	OR                 { TokenOR _ }
	AND                { TokenAND _ }
	NOT                { TokenNOT _ }
	"["                { TokenBracketOpen _ }
	"]"                { TokenBracketClose _ }
	"("                { TokenParenOpen _ }
	")"                { TokenParenClose _ }
	TAUTOLOGY          { TokenTautology _ }
	CONTRADICTION      { TokenContradiction _ }
	FOR_ALL            { TokenForAll _ }
	THERE_EXISTS       { TokenExists _ }
	":"                { TokenColon _ }
	","                { TokenComma _ }
	"->"               { TokenImplies _ }
	VARIABLE           { TokenVariable _ $$ }
	PREDICATE          { TokenPredicate _ $$ }
	NEWLINE            { TokenNewline _ }

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
	: arg                                   { [$1] }
	| argList "," arg                       { $1 ++ [$3] }

arg: VARIABLE                               { Variable $1 }

optCOLON:   { Nil } | ":"     { $1 }
optNEWLINE: { Nil } | NEWLINE { $1 }


{

-- NODES --
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

data Variable
	= Variable String
	deriving (Show, Eq)

type ArgList = [Variable]


-- HELPERS --
freeVariables :: Formula -> [Variable]
freeVariables (Formula f) = freeVariables f
freeVariables (Or a b) = union (freeVariables a) (freeVariables b)
freeVariables (And a b) = union (freeVariables a) (freeVariables b)
freeVariables (Not f) = freeVariables f
freeVariables (Implication a b) = union (freeVariables a) (freeVariables b)
freeVariables (UniversalQuantifier vars f) = (freeVariables f) \\ vars
freeVariables (ExistentialQuantifier vars f) = (freeVariables f) \\ vars
freeVariables (Tautology t) = []
freeVariables (Contradiction f) = []
freeVariables (Atomic predicate vars) = nub vars

isSentence :: Formula -> Bool
isSentence (Formula f) = case (freeVariables f) of
	[] -> True
	_ -> False


-- MAIN --

prettyPrintArray arr = "[ " ++ (foldr (\a b -> case b of; [] -> a; _ -> a ++ "\n\n, " ++ b) "" (map show arr)) ++ "\n]"

main = do
	s <- getContents
	let parseTrees = generate (scanTokens s)
	let arrFreeVariables = map (map (\(Variable s) -> s)) (map freeVariables parseTrees)
	let arrIsSentence = map isSentence parseTrees
	let zipped = (zip3 parseTrees arrFreeVariables arrIsSentence)
	putStrLn (prettyPrintArray zipped)

parseError :: [Token] -> a
parseError tokenList = let pos = tokenPosn(head(tokenList))
	in
	error ("parse error: unexpected " ++ showToken(head(tokenList)) ++ " at line " ++ show(getLineNum(pos)) ++ ", column " ++ show(getColumnNum(pos)))

}
