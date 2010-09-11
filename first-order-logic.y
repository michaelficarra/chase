{
module Main where
import Lexer
import Word
import Data.List
import Data.Maybe
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
	: expr                                  { $1 }
	| expr "->" expr                        { Implication $1 $3 }

quantifierBody: optCOLON expr               { $2 }

expr
	: exprOR                                { $1 }
	| FOR_ALL argList quantifierBody        { UniversalQuantifier $2 $3 }
	| THERE_EXISTS argList quantifierBody   { ExistentialQuantifier $2 $3 }

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

type DomainElement = Word
type Domain = [DomainElement]
type Relation = (String, Int, [[DomainElement]])
type Model = (Domain,[Relation])
type Environment = [(Variable,DomainElement)]

mkDomain :: Integer -> [DomainElement]
mkDomain size = [1..(fromIntegral size)]
mkRelation name arity truthTable = (name,(fromIntegral arity),truthTable)
mkModel domain relations = (domain,relations)


-- HELPERS --

variables :: Formula -> [Variable]
-- return an array of all free and bound variables
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
-- return an array of all free variables
freeVariables formula = case formula of
	Or a b -> union (freeVariables a) (freeVariables b)
	And a b -> union (freeVariables a) (freeVariables b)
	Not f -> freeVariables f
	Implication a b -> union (freeVariables a) (freeVariables b)
	UniversalQuantifier vars f -> (freeVariables f) \\ vars
	ExistentialQuantifier vars f -> (freeVariables f) \\ vars
	Atomic predicate vars -> nub vars
	_ -> []

variableName :: Variable -> String
-- returns the internal string value of a given variable
variableName v = (\(Variable v') -> v') v

isSentence :: Formula -> Bool
-- returns true if all variables in the given formula are bound, false otherwise
isSentence f = case (freeVariables f) of; [] -> True; _ -> False

variant :: Variable -> [Variable] -> Variable
-- takes a variable and a blacklist of variables and returns a variable that is
-- not in the blacklist
variant x vars = if x `elem` vars then variant (Variable ((variableName x) ++ "'")) vars else x

len :: Formula -> Integer
-- calculates a "length" of a formula
len formula = case formula of
	Or a b -> 1 + (len a) + (len b)
	And a b -> 1 + (len a) + (len b)
	Not f -> 1 + (len f)
	Implication a b -> 1 + (len a) + (len b)
	UniversalQuantifier vars f -> len f
	ExistentialQuantifier vars f -> len f
	_ -> 1

isPEF :: Formula -> Bool
-- determines whether a given formula is in positive existential form
isPEF formula = case formula of
	Or a b -> (isPEF a) && (isPEF b)
	And a b -> (isPEF a) && (isPEF b)
	ExistentialQuantifier v f -> isPEF f
	Atomic p v -> True
	Tautology -> True
	Contradiction -> True
	_ -> False

parseRelations :: [String] -> [Relation]
-- takes an array of Strings that represent the truth of a relation and returns
-- an array of Relations
parseRelations strings = foldl (\a b -> mergeRelation b a) [] (map parseRelation strings)

parseRelation :: String -> Relation
-- returns a relation represented by the given string
parseRelation str =
	mkRelation (takeWhile predicateSep str) (length argList) argList
	where
		predicateSep = (\a -> a /= '(' && a /= '[')
		argList = [map read (split ',' (init.tail $ dropWhile predicateSep str))]

mergeRelation :: Relation -> [Relation] -> [Relation]
-- takes a relation and a list of relations and adds the relation to the list,
-- only adding its arguments if the predicate already exists
mergeRelation relation [] = [relation]
mergeRelation relation (r:rs)
	| rPredicate == relationPredicate && rArity == relationArity =
		(rPredicate, rArity, nub (rTruthTable ++ relationTruthTable)) : rs
	| otherwise = r : (mergeRelation relation rs)
	where
		(rPredicate,rArity,rTruthTable) = r
		(relationPredicate,relationArity,relationTruthTable) = relation

parseModel :: String -> Model
-- returns a model when given a string representation of a model
parseModel str =
	mkModel (mkDomain $ fromIntegral domainSize) (parseRelations relations)
	where
		fileLines = lines $ str
		domainSize = read.head $ fileLines :: DomainElement
		relations = tail fileLines

split :: Eq a => a -> [a] -> [[a]]
-- split an array by an element of that array
split delim [] = [[]]
split delim (c:cs)
	| c == delim = [] : others
	| otherwise = (c : head others) : tail others
	where
		others = split delim cs

lookup2 :: Eq a => Eq b => a -> b -> [(a,b,c)] -> Maybe c
lookup2 a b abc = lookup (a,b) (map (\(a,b,c) -> ((a,b),c)) abc)

hashSet :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
-- like setting the value of an element of a hash
hashSet [] v v' = [(v,v')]
hashSet (k:ks) v v'
	| fst k == v = (v,v') : ks
	| otherwise = k : hashSet ks v v'


-- SIMPLIFICATION / REWRITING --

recursivelyApply :: (Formula -> Formula) -> Formula -> Formula
-- helps when writing rewriters, as it handles formula walking and applies the
-- rewriting function to all nodes
recursivelyApply fn formula = case formula of
	And a b -> fn $ And (recursivelyApply fn a) (recursivelyApply fn b)
	Or a b -> fn $ Or (recursivelyApply fn a) (recursivelyApply fn b)
	Not f -> fn $ Not (recursivelyApply fn f)
	Implication a b -> fn $ Implication (recursivelyApply fn a) (recursivelyApply fn b)
	UniversalQuantifier v f -> fn $ UniversalQuantifier v (recursivelyApply fn f)
	ExistentialQuantifier v f -> fn $ ExistentialQuantifier v (recursivelyApply fn f)
	Atomic p v -> fn $ Atomic p v
	Tautology -> fn $ Tautology
	Contradiction -> fn $ Contradiction

replaceVariables :: [Variable] -> Formula -> Formula
-- takes a blacklist of variables and a formula and replaces all occurrences of
-- any of the variables with a safe alternative
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

substitute :: [(Variable,Variable)] -> Formula -> Formula
-- for each tuple (<a>,<b>) in the first argument, recursively replaces
-- references to <a> with <b>; replacement is done without regard to logical
-- consistency
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
		sub list = (map (\v -> fromMaybe v (lookup v pairs)) list)

pullQuantifiers :: Formula -> Formula
-- pulls the quantifiers in the given formula from any sub-formula to the
-- outermost formula
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
-- a function used for mutual recursion with pullQuantifiers
pullQuantifier (l,r) formula operator quantifier v1 f1 v2 f2 =
	quantifier z (pullQuantifiers (operator a' b'))
	where
		z = map (\s -> variant s (freeVariables formula)) v1
		a' = if l then substitute (filter (\p -> (fst p) /= (snd p)) (zip v1 z)) f1 else f1
		b' = if r then substitute (filter (\p -> (fst p) /= (snd p)) (zip v2 z)) f2 else f2

prenex :: Formula -> Formula
-- applies pullQuantifiers to a given formula and its subformulas
prenex formula = case formula of
	And a b -> pullQuantifiers $ And (prenex a) (prenex b)
	Or a b -> pullQuantifiers $ Or (prenex a) (prenex b)
	UniversalQuantifier v f -> UniversalQuantifier v (prenex f)
	ExistentialQuantifier v f -> ExistentialQuantifier v (prenex f)
	_ -> formula

nnf :: Formula -> Formula
-- nnf = Negation Normal Form
-- converts an arbitrary formula to negation normal form
nnf formula = recursivelyApply (\formula -> case formula of
	Not (Implication a b) -> And a (Not b)
	Implication a b -> Or (Not a) b
	Not (Not f) -> f
	Not (Or a b) -> And (Not a) (Not b)
	Not (And a b) -> Or (Not a) (Not b)
	Not (UniversalQuantifier vars f) -> ExistentialQuantifier vars (Not f)
	Not (ExistentialQuantifier vars f) -> UniversalQuantifier vars (Not f)
	_ -> formula
	) formula

pnf :: Formula -> Formula
-- pnf = Prenex Normal Form
-- converts an arbitrary formula to prenex normal form
pnf formula = prenex.nnf.uselessQuantifiers $ formula

pef :: Formula -> Formula
-- attempts to convert a formula to positive existential form
pef formula = uselessQuantifiers.pullQuantifiers $ recursivelyApply (\formula -> case formula of
	And (Not a) (Not b) -> Or a b
	Or (Not a) (Not b) -> And a b
	Not (Not f) -> f
	Implication (Not a) b -> Or a b
	Implication a (Not b) -> And a b
	Not (UniversalQuantifier v (Not f)) -> ExistentialQuantifier v f
	-- the rest of these exist just to ensure the error condition is not met
	-- basically, they are the nodes that are naturally allowed
	ExistentialQuantifier v f -> ExistentialQuantifier v f
	And a b -> And a b
	Or a b -> Or a b
	Atomic p v -> Atomic p v
	Contradiction -> Contradiction
	Tautology -> Tautology
	_ -> error("Unable to convert (" ++ (show formula) ++ ") to Positive Existential Form")
	) (simplify formula)

doubleNegation :: Formula -> Formula
-- a simplifier that removes all double-negations from a given formula
doubleNegation formula = recursivelyApply (\formula -> case formula of
	Not (Not f) -> f
	_ -> formula
	) formula

deMorgan :: Formula -> Formula
-- a simplifier that applies deMorgan's laws to a given formula
deMorgan formula = recursivelyApply (\formula -> case formula of
	Not (And a b) -> Or (Not a) (Not b)
	Not (Or a b) -> And (Not a) (Not b)
	Not (UniversalQuantifier vars f) -> ExistentialQuantifier vars (Not f)
	Not (ExistentialQuantifier vars f) -> UniversalQuantifier vars (Not f)
	_ -> formula
	) formula

uselessQuantifiers :: Formula -> Formula
-- a simplifier that removes unnecessary quantifiers and combines bound
-- variables in adjacent quantifiers
uselessQuantifiers formula = recursivelyApply (\formula -> case formula of
	Not (UniversalQuantifier v (Not f)) -> ExistentialQuantifier v f
	Not (ExistentialQuantifier v (Not f)) -> UniversalQuantifier v f
	(UniversalQuantifier v1 (UniversalQuantifier v2 f)) -> UniversalQuantifier (union v1 v2) f
	(ExistentialQuantifier v1 (ExistentialQuantifier v2 f)) -> ExistentialQuantifier (union v1 v2) f
	(UniversalQuantifier vars f) ->
		let intersection = (intersect vars (freeVariables f)) in
		if (null intersection) then f
		else UniversalQuantifier intersection f
	(ExistentialQuantifier vars f) ->
		let intersection = (intersect vars (freeVariables f)) in
		if (null intersection) then f
		else ExistentialQuantifier intersection f
	_ -> formula
	) formula

tautologies :: Formula -> Formula
tautologies formula = recursivelyApply (\formula -> case formula of
	And a Tautology -> a
	And Tautology b -> b
	And a Contradiction -> Contradiction
	And Contradiction b -> Contradiction
	Or a Tautology -> Tautology
	Or Tautology b -> Tautology
	Or a Contradiction -> a
	Or Contradiction b -> b
	Not Tautology -> Contradiction
	Not Contradiction -> Tautology
	Implication Tautology b -> b
	Implication Contradiction b -> Tautology
	Implication a Tautology -> Tautology
	Implication a Contradiction -> Not a
	_ -> formula
	) formula

simplify :: Formula -> Formula
-- applies a preselected list of simplifications to a given formula
simplify f = foldl (\a b -> b a) f (concat $ permutations simplifiers)
	where simplifiers = [uselessQuantifiers,deMorgan,doubleNegation,tautologies]


-- I/O --

loadModel :: String -> IO String
-- when given the name of a stored model, returns a lazy string representation of it
loadModel fileName = readFile ("./models/" ++ fileName)

showModel :: Model -> String
-- nicely outputs a Model
showModel (domain,relations) = "( domain: 1.." ++ (show.length $ domain) ++ ", relations: [" ++ (intercalate ", " truths) ++ "] )"
	where
		truths = concat $ map (\(predicate,arity,arrVars) ->
				map (\vars -> predicate ++ "[" ++ intercalate "," (map show vars) ++ "]") arrVars
			) relations

showFormula :: Formula -> String
-- builds a human-readable string representation of a formula
showFormula formula = case formula of
	And a b -> (showFormula a) ++ " ∧ " ++ (showFormula b)
	Or a b -> "(" ++ (showFormula a) ++ " ∨ " ++ (showFormula b) ++ ")"
	Not f -> "¬" ++ (showFormula f)
	Implication a b -> (showFormula a) ++ " → " ++ (showFormula b)
	UniversalQuantifier v f -> "∀ " ++ (intercalate "," (map variableName v)) ++ ": " ++ (showFormula f)
	ExistentialQuantifier v f -> "∃ " ++ (intercalate "," (map variableName v)) ++ ": " ++ (showFormula f)
	Atomic p v -> p ++ "[" ++ (intercalate "," (map variableName v)) ++ "]"
	Tautology -> "⟙"
	Contradiction -> "⟘"
	_ -> "[?]"


-- MAIN --

holds :: Model -> Environment -> Formula -> Bool
holds model env formula = let (domain,relations) = model in case formula of
	Contradiction -> False
	Tautology -> True
	Atomic predicate vars -> (map (\v -> case lookup v env of
		Just v' -> v'
		Nothing -> error ("Could not look up variable \"" ++ variableName v ++ "\" in environment " ++ show env ++ " for formula " ++ showFormula formula)
		) vars) `elem` (fromMaybe [] (lookup2 predicate (length vars) relations))
	Or a b -> holds model env a || holds model env b
	And a b -> holds model env a && holds model env b
	Not f -> not $ holds model env f
	Implication a b -> holds model env b || not (holds model env a)
	UniversalQuantifier [] f -> holds model env f
	UniversalQuantifier (v:vs) f -> all (\v' -> holds model (hashSet env v v') (UniversalQuantifier vs f)) domain
	ExistentialQuantifier [] f -> holds model env f
	ExistentialQuantifier (v:vs) f -> any (\v' -> holds model (hashSet env v v') (ExistentialQuantifier vs f)) domain

expandDomain :: Model -> Model
expandDomain (domain,relations) = (domain',relations)
	where domain' = mkDomain (1 + (fromIntegral $ length domain))

expandRelations :: Model -> Formula -> Model
expandRelations (domain,relations) formula = let model = (domain,relations) in
	case formula of
		And a b -> expandRelations (expandRelations model a) b
		Or a b -> expandRelations (expandRelations model a) b
		Implication a b -> expandRelations (expandRelations model a) b
		Atomic p v -> (domain,relations')
		_ -> model
	where relations' = relations -- TODO: implement

satisfyModel :: Model -> Formula -> Model
satisfyModel model formula =
	if holds model [] formula then model
	else satisfyModel (expandRelations (expandDomain model) formula) formula

chase :: [Formula] -> Model
chase formulas = runChase ([],[]) formulas

runChase :: Model -> [Formula] -> Model
runChase model formulas = if all (\formula -> case formula of
		Implication a b ->
			let f = (Implication a b) in
			let f' = UniversalQuantifier (freeVariables f) f in
			if (isPEF a) && (isPEF b) then holds model [] f'
			else error "All formulas given to the `chase` function must be in positive existential form"
		_ ->
			if (isPEF formula) then True -- holds model [] (UniversalQuantifier (freeVariables formula) formula)
			else error "All formulas given to the `chase` function must be an implication of positive existential formulas"
	) formulas then model
	else runChase (foldl satisfyModel model formulas) formulas

main = do
	formulae <- getContents
	let parseTrees = generate (scanTokens formulae)
	-- putStrLn $ prettyPrintArray (map showFormula parseTrees)

	modelAStr <- loadModel "A"
	let modelA = parseModel modelAStr
	putStrLn $ "model A: " ++ showModel modelA

	-- random tests / sanity checks
	putStrLn.showFormula $ simplify (And (ExistentialQuantifier [Variable "x"] Tautology) (Not (UniversalQuantifier [Variable "y"] (Not (Contradiction)))))
	-- putStrLn.show $ holds modelA [(Variable "x",0)] (ExistentialQuantifier [Variable "y"] (Atomic "R" [Variable "x", Variable "y"]))
	-- putStrLn.showFormula $ pef (And (ExistentialQuantifier [Variable "x"] Tautology) (Not (UniversalQuantifier [Variable "y"] (Not (Atomic "R" [Variable "y",Variable "z"])))))
	-- putStrLn.showFormula.simplify $ nnf (And (ExistentialQuantifier [Variable "x"] Tautology) (Not (UniversalQuantifier [Variable "y"] (Not (Atomic "R" [Variable "y",Variable "z"])))))
	-- putStrLn.show $ all (\formula -> holds (mkModel [0,1,2] [("R",[[0,1]]),("Q",[[1,2]])]) [] formula) theory

	-- chase function tests
	putStrLn "--- chase ---"
	putStrLn.prettyPrintArray $ map showFormula theory
	putStrLn.showModel $ chase theory

	where
		prettyPrintArray arr = "[ " ++ (intercalate "\n, " arr) ++ "\n]"
		theory = map (head.generate.scanTokens) [
			"Tautology -> Exists y,y': R[y,y']",
			"R[x,w] -> Exists y: Q[x,y]",
			"Q[u,v] -> Exists z: R[v,z]"
			]

parseError :: [Token] -> a
parseError tokenList =
	let pos = tokenPosn(head(tokenList)) in
	error ("parse error: unexpected " ++ showToken(head(tokenList)) ++ " at line " ++ show(getLineNum(pos)) ++ ", column " ++ show(getColumnNum(pos)))

}
