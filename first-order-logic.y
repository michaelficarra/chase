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
	| TAUTOLOGY                             { Tautology }
	| CONTRADICTION                         { Contradiction }
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
	| Tautology
	| Contradiction
	| Atomic String ArgList
	deriving (Show, Eq)

data Variable
	= Variable String
	deriving (Show, Eq)

type ArgList = [Variable]

mkOr a b = Or a b
mkAnd a b = And a b
mkNot f = Not f
mkImplication a b = Implication a b
mkUniversalQuantifier v f = UniversalQuantifier v f
mkExistentialQuantifier v f = ExistentialQuantifier v f
mkTautology = Tautology
mkContradiction = Contradiction
mkAtomic p v = Atomic p v
mkVariable v = Variable v


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

isPEF :: Formula -> Bool
isPEF formula = case formula of
	Or a b -> (isPEF a) && (isPEF b)
	And a b -> (isPEF a) && (isPEF b)
	ExistentialQuantifier v f -> isPEF f
	Atomic p v -> True
	Tautology -> True
	Contradiction -> True
	_ -> False


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
	where
		sub list = (map (\v -> case (lookup v pairs) of; Just v' -> v'; _ -> v) list)

pullQuantifiers :: Formula -> Formula
pullQuantifiers formula = case formula of
	And (UniversalQuantifier v1 f1)   (UniversalQuantifier v2 f2)   -> pullQuantifier (True,True) formula mkAnd mkUniversalQuantifier   v1 f1 v2 f2
	Or  (ExistentialQuantifier v1 f1) (ExistentialQuantifier v2 f2) -> pullQuantifier (True,True) formula mkOr  mkExistentialQuantifier v1 f1 v2 f2
	And (UniversalQuantifier v1 f1)   b -> pullQuantifier (True,False) formula mkAnd mkUniversalQuantifier   v1 f1 v1 b
	And a   (UniversalQuantifier v2 f2) -> pullQuantifier (False,True) formula mkAnd mkUniversalQuantifier   v2 a v2 f2
	Or  (UniversalQuantifier v1 f1)   b -> pullQuantifier (True,False) formula mkOr  mkUniversalQuantifier   v1 f1 v1 b
	Or  a   (UniversalQuantifier v2 f2) -> pullQuantifier (False,True) formula mkOr  mkUniversalQuantifier   v2 a v2 f2
	And (ExistentialQuantifier v1 f1) b -> pullQuantifier (True,False) formula mkAnd mkExistentialQuantifier v1 f1 v1 b
	And a (ExistentialQuantifier v2 f2) -> pullQuantifier (False,True) formula mkAnd mkExistentialQuantifier v2 a v2 f2
	Or  (ExistentialQuantifier v1 f1) b -> pullQuantifier (True,False) formula mkOr  mkExistentialQuantifier v1 f1 v1 b
	Or  a (ExistentialQuantifier v2 f2) -> pullQuantifier (False,True) formula mkOr  mkExistentialQuantifier v2 a v2 f2
	_ -> formula

pullQuantifier :: (Bool,Bool) -> Formula -> (Formula -> Formula -> Formula) -> ([Variable] -> Formula -> Formula) -> [Variable] -> Formula -> [Variable] -> Formula -> Formula
pullQuantifier (l,r) formula operator quantifier v1 f1 v2 f2 =
	quantifier z (pullQuantifiers (operator a' b'))
	where
		z = map (\s -> variant s (freeVariables formula)) v1
		a' = if l then substitute (zip v1 z) f1 else f1
		b' = if r then substitute (zip v2 z) f2 else f2

prenex :: Formula -> Formula
prenex formula = case formula of
	And a b -> pullQuantifiers $ And (prenex a) (prenex b)
	Or a b -> pullQuantifiers $ Or (prenex a) (prenex b)
	UniversalQuantifier v f -> UniversalQuantifier v (prenex f)
	ExistentialQuantifier v f -> ExistentialQuantifier v (prenex f)
	_ -> formula

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

-- pnf = Prenex Normal Form
pnf :: Formula -> Formula
pnf formula = prenex . nnf . uselessQuantifiers $ formula

{-
-- pef = Positive Existential Form
pef :: Formula -> Maybe Formula
pef formula = 
-}

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

{-
chase :: Formula -> [(a,b)] -> Maybe Model
chase (Implication a b) =
	| (isPEF a) && (isPEF b) = Model
	| otherwise = error "Both arguments to the `chase` function must be in Positive Existential Form"
-}

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
