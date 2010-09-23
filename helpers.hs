module Helpers where
import Parser
import Data.List
import Data.Maybe

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
-- return an array of all free variables in given formula
freeVariables formula = case formula of
	Or a b -> union (freeVariables a) (freeVariables b)
	And a b -> union (freeVariables a) (freeVariables b)
	Not f -> freeVariables f
	Implication a b -> union (freeVariables a) (freeVariables b)
	UniversalQuantifier vars f -> (freeVariables f) \\ vars
	ExistentialQuantifier vars f -> (freeVariables f) \\ vars
	Atomic predicate vars -> nub vars
	_ -> []

boundVariables :: Formula -> [Variable]
-- return an array of all bound variables in given formula
boundVariables formula = variables formula \\ freeVariables formula

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
	let predicateSep = (\a -> a /= '(' && a /= '[') in
	let argList = map read (split ',' (init.tail $ dropWhile predicateSep str)) in
	mkRelation (takeWhile predicateSep str) (length argList) [argList]

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
	let fileLines = lines $ str in
	let domainSize = read.head $ fileLines :: DomainElement in
	let relations = tail fileLines in
	mkModel (mkDomain domainSize) (parseRelations relations)

split :: Eq a => a -> [a] -> [[a]]
-- split an array by an element of that array
split delim [] = [[]]
split delim (c:cs)
	| c == delim = [] : others
	| otherwise = (c : head others) : tail others
	where
		others = split delim cs

lookup2 :: Eq a => Eq b => a -> b -> [(a,b,c)] -> Maybe c
-- like Data.List.lookup, but with a two-value key on a three-value tuple
lookup2 a b abc = lookup (a,b) (map (\(a,b,c) -> ((a,b),c)) abc)

hashSet :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
-- like setting the value of an element of a hash
hashSet [] k v = [(k,v)]
hashSet (x:xs) k v
	| fst x == k = (k,v) : xs
	| otherwise = x : hashSet xs k v


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
	let z = map (\s -> variant s (freeVariables formula)) v1 in
	let a' = if l then substitute (filter (\p -> (fst p) /= (snd p)) (zip v1 z)) f1 else f1 in
	let b' = if r then substitute (filter (\p -> (fst p) /= (snd p)) (zip v2 z)) f2 else f2 in
	quantifier z (pullQuantifiers (operator a' b'))

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
-- removes tautological expressions stemming from Boolean operations being
-- performed on truth or falsehood constants
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
simplify f =
	let simplifiers = [uselessQuantifiers,deMorgan,doubleNegation,tautologies] in
	foldl (\a b -> b a) f (concat $ permutations simplifiers)


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
	And a b -> "(" ++ (showFormula a) ++ " & " ++ (showFormula b) ++ ")"
	Or a b -> "(" ++ (showFormula a) ++ " | " ++ (showFormula b) ++ ")"
	Not f -> "!" ++ (showFormula f)
	Implication a b -> "(" ++ (showFormula a) ++ ")" ++ " -> " ++ "(" ++ (showFormula b) ++ ")"
	UniversalQuantifier v f -> "ForAll " ++ (intercalate "," (map variableName v)) ++ ": (" ++ (showFormula f) ++ ")"
	ExistentialQuantifier v f -> "Exists " ++ (intercalate "," (map variableName v)) ++ ": (" ++ (showFormula f) ++ ")"
	Atomic p v -> p ++ "[" ++ (intercalate "," (map variableName v)) ++ "]"
	Tautology -> "True"
	Contradiction -> "False"
	_ -> "[?]"

exportModel :: Model -> String
-- converts a model to a String that is in an easily parseable, yet
-- human-readable format
exportModel (domain,relations) =
	(show$length domain) ++
	concatMap (\(predicate,arity,truthTable) ->
		concatMap (\args ->
			"\n" ++ predicate ++ "[" ++ (intercalate "," (map show args)) ++ "]"
		) truthTable
	) relations

writeModelsToFiles :: String -> [Model] -> IO ()
-- outputs the given list of models to files in the given directory in a format
-- as defined by `exportModel`
writeModelsToFiles directory models = do
	head $
		map (\(modelNumber,model) ->
				let file = directory ++ "/" ++ (show modelNumber) in
				writeFile file (exportModel model)
			)
		$ zip (iterate (+1) 0) models


-- MAIN --

holds :: Model -> Formula -> Bool
-- determines if the given formula holds in the given model
-- note: formula given will not be universally quantified, so unbound variable
--       references will cause an error
holds model formula = holds' model [] formula

holds' :: Model -> Environment -> Formula -> Bool
-- used by `holds` to hide the environment identity argument
holds' model env formula = let (domain,relations) = model in let self = holds' model in case formula of
	Contradiction -> False
	Tautology -> True
	Atomic predicate vars -> (map (\v -> case lookup v env of
		Just v' -> v'
		Nothing -> error ("Could not look up variable \"" ++ variableName v ++ "\" in environment " ++ show env ++ " for atomic " ++ showFormula formula)
		) vars) `elem` (fromMaybe [] (lookup2 predicate (length vars) relations))
	Or a b -> self env a || self env b
	And a b -> self env a && self env b
	Not f -> not $ self env f
	Implication a b -> self env b || not (self env a)
	UniversalQuantifier [] f -> self env f
	UniversalQuantifier (v:vs) f -> all (\v' -> self (hashSet env v v') (UniversalQuantifier vs f)) domain
	ExistentialQuantifier [] f -> self env f
	ExistentialQuantifier (v:vs) f -> any (\v' -> self (hashSet env v v') (ExistentialQuantifier vs f)) domain
