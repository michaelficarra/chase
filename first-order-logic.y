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
	: formula                               { [$1] }
	| formulaList NEWLINE formula           { $1 ++ [$3] }

formula
	: expr                                      { $1 }
	| expr "->" expr                            { Implication $1 $3 }
	| FOR_ALL argList quantifierBody            { UniversalQuantifier $2 $3 }
	| THERE_EXISTS argList quantifierBody       { ExistentialQuantifier $2 $3 }

quantifierBody: optCOLON formula                { $2 }

expr: exprOR { $1 }

exprOR
	: exprAND                               { $1 }
	| exprOR OR exprAND                     { Or $1 $3 }

exprAND
	: exprValue                             { $1 }
	| exprAND AND exprValue                 { And $1 $3 }

exprValue
	: atomic                                { $1 }
	| "(" formula ")"                       { $2 }
	| "[" formula "]"                       { $2 }
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
	= Or Formula Formula
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
variables :: Formula -> [Variable]
variables formula = case formula of
	Or a b -> union (variables a) (variables b)
	And a b -> union (variables a) (variables b)
	Not f -> variables f
	Implication a b -> union (variables a) (variables b)
	UniversalQuantifier vars f -> union vars (variables f)
	ExistentialQuantifier vars f -> union vars (variables f)
	Atomic predicate vars -> nub vars
	_ -> []

freeVariables :: Formula -> [Variable]
freeVariables formula = case formula of
	Or a b -> union (freeVariables a) (freeVariables b)
	And a b -> union (freeVariables a) (freeVariables b)
	Not f -> freeVariables f
	Implication a b -> union (freeVariables a) (freeVariables b)
	UniversalQuantifier vars f -> (freeVariables f) \\ vars
	ExistentialQuantifier vars f -> (freeVariables f) \\ vars
	Atomic predicate vars -> nub vars
	_ -> []

isSentence :: Formula -> Bool
isSentence f = case (freeVariables f) of; [] -> True; _ -> False

variant :: Variable -> [Variable] -> Variable
variant x vars = if x `elem` vars then variant (Variable (((\(Variable v) -> v) x) ++ "\'")) vars else x

len :: Formula -> Integer
len formula = case formula of
	Or a b -> 1 + (len a) + (len b)
	And a b -> 1 + (len a) + (len b)
	Not f -> 1 + (len f)
	Implication a b -> 1 + (len a) + (len b)
	UniversalQuantifier vars f -> len f
	ExistentialQuantifier vars f -> len f
	_ -> 1


-- SIMPLIFICATION / REWRITING --

-- takes a blacklist of variables and a formula, and replaces all
-- occurrences of any of the variables with a safe alternative
replaceVariables :: [Variable] -> Formula -> Formula
replaceVariables vars formula = case formula of
	Or a b -> Or (replaceVariables vars a) (replaceVariables vars b)
	And a b -> And (replaceVariables vars a) (replaceVariables vars b)
	Implication a b -> Implication (replaceVariables vars a) (replaceVariables vars b)
	Not f -> Not (replaceVariables vars f)
	UniversalQuantifier v f ->
		let v' = map ($ (variables f)) (map variant v) in
		UniversalQuantifier v' (replaceVariables (union v' vars) (substitute (zip v v') f))
	ExistentialQuantifier v f ->
		let v' = map ($ (variables f)) (map variant v) in
		ExistentialQuantifier v' (replaceVariables (union v' vars) (substitute (zip v v') f))
	Atomic p v ->
		let v' = map ($ vars) (map variant v) in
		Atomic p v'
	_ -> formula

-- for each tuple (<a>,<b>) in the first argument, recursively
-- replaces references to <a> with <b>; replacement is done without
-- regard to logical consistency
substitute :: [(Variable,Variable)] -> Formula -> Formula
substitute pairs formula = case formula of
	Or a b -> Or (substitute pairs a) (substitute pairs b)
	And a b -> And (substitute pairs a) (substitute pairs b)
	Implication a b -> Implication (substitute pairs a) (substitute pairs b)
	Not f -> Not (substitute pairs f)
	UniversalQuantifier v f -> UniversalQuantifier (sub v) f
	ExistentialQuantifier v f -> ExistentialQuantifier (sub v) f
	Atomic p v -> Atomic p (sub v)
	_ -> formula
	where sub = listSubstitute pairs

listSubstitute :: Eq a => [(a,a)] -> [a] -> [a]
listSubstitute subs list = (map (\v -> case (lookup v subs) of; Just v' -> v'; _ -> v) list)

-- nnf = Negation Normal Form
nnf :: Formula -> Formula
nnf formula = case formula of
	Or a b -> Or (nnf a) (nnf b)
	And a b -> And (nnf a) (nnf b)
	Implication a b -> Or (nnf (Not a)) (nnf b)
	Not (Not f) -> nnf f
	Not (Or a b) -> And (nnf (Not a)) (nnf (Not b))
	Not (And a b) -> Or (nnf (Not a)) (nnf (Not b))
	Not (Implication a b) -> And (nnf a) (nnf (Not b))
	UniversalQuantifier vars f -> UniversalQuantifier vars (nnf f)
	ExistentialQuantifier vars f -> ExistentialQuantifier vars (nnf f)
	Not (UniversalQuantifier vars f) -> UniversalQuantifier vars (nnf (Not f))
	Not (ExistentialQuantifier vars f) -> ExistentialQuantifier vars (nnf (Not f))
	_ -> formula

doubleNegation :: Formula -> Formula
doubleNegation formula = case formula of
	Not (Not f) -> doubleNegation f
	_ -> formula

deMorgan :: Formula -> Formula
deMorgan formula = case formula of
	Not (And a b) -> Or (deMorgan (Not a)) (deMorgan (Not b))
	Not (Or a b) -> And (deMorgan (Not a)) (deMorgan (Not b))
	Not (UniversalQuantifier vars f) -> ExistentialQuantifier vars (deMorgan (Not f))
	Not (ExistentialQuantifier vars f) -> UniversalQuantifier vars (deMorgan (Not f))
	_ -> formula

uselessQuantifiers :: Formula -> Formula
uselessQuantifiers formula = case formula of
	(UniversalQuantifier v1 (UniversalQuantifier v2 f)) -> UniversalQuantifier (union v1 v2) f
	(ExistentialQuantifier v1 (ExistentialQuantifier v2 f)) -> ExistentialQuantifier (union v1 v2) f
	(UniversalQuantifier vars f) ->
		let intersection = (intersect vars (freeVariables f)) in
		if (null intersection) then (uselessQuantifiers f)
		else UniversalQuantifier intersection (uselessQuantifiers f)
	(ExistentialQuantifier vars f) ->
		let intersection = (intersect vars (freeVariables f)) in
		if (null intersection) then (uselessQuantifiers f)
		else ExistentialQuantifier intersection (uselessQuantifiers f)
	Or a b -> Or (uselessQuantifiers a) (uselessQuantifiers b)
	And a b -> And (uselessQuantifiers a) (uselessQuantifiers b)
	Implication a b -> Implication (uselessQuantifiers a) (uselessQuantifiers b)
	Not f -> Not (uselessQuantifiers f)
	_ -> formula

simplify :: Formula -> Formula
simplify f = foldl (\a b -> b a) f [uselessQuantifiers,doubleNegation,deMorgan]

-- MAIN --

prettyPrintArray arr = "[ " ++ (concat (intersperse "\n, " (map show arr))) ++ "\n]"

main = do
	s <- getContents
	let parseTrees = generate (scanTokens s)
	let arrFreeVariables = map (map (\(Variable s) -> s)) (map freeVariables parseTrees)
	let arrIsSentence = map isSentence parseTrees
	let arrLen = map len parseTrees
	let arrNNF = map nnf parseTrees
	let arrSimplify = map simplify parseTrees
	putStrLn (prettyPrintArray parseTrees)

parseError :: [Token] -> a
parseError tokenList =
	let pos = tokenPosn(head(tokenList)) in
	error ("parse error: unexpected " ++ showToken(head(tokenList)) ++ " at line " ++ show(getLineNum(pos)) ++ ", column " ++ show(getColumnNum(pos)))

}
