module Chase where
import Parser
import Helpers
import qualified Debug.Trace
import Data.List

-- trace x = id
trace = Debug.Trace.trace

verify :: Formula -> Formula
-- verifies that a formula is in positive existential form and performs some
-- normalization on implied/constant implications
verify formula = case formula of
	Implication a b -> Implication (pef a) (pef b)
	Not f -> Implication (pef f) Contradiction
	_ -> Implication Tautology (pef formula)

order :: [Formula] -> [Formula]
-- 
order formulae = sortBy (\a b ->
	let extractRHS = (\f -> case f of; (Implication lhs rhs) -> rhs; _ -> f) in
	let (rhsA,rhsB) = (extractRHS a, extractRHS b) in
	let (lenA,lenB) = (numDisjuncts rhsA, numDisjuncts rhsB) in
	let (vA,vB) = (length (variables a), length (variables b)) in
	if lenA == lenB || lenA > 1 && lenB > 1 then
		if vA == vB then EQ
		else
			if vA < vB then LT
			else GT
	else
		if lenA < lenB then LT
		else GT
	) formulae

chase :: [Formula] -> [Model]
-- a wrapper for the chase' function to hide the model identity and theory
-- manipulation
chase theory = nub $ chase' (order $ map verify theory) [([],[])]

chase' :: [Formula] -> [Model] -> [Model]
-- runs the chase algorithm on a given theory, manipulating the given list of
-- models, and returning a list of models that satisfy the theory
chase' _ [] = trace "  but it is impossible to make the model satisfy the theory by adding to it" []
chase' theory pending = concatMap (branch theory) pending

branch :: [Formula] -> Model -> [Model]
-- 
branch theory model =
	let reBranch = chase' theory in
	trace ("running chase on " ++ show model) $
	case findFirstFailure model theory of
		Just newModels ->
			trace ("  at least one formula does not hold for model " ++ showModel model) $
			reBranch newModels
		Nothing -> -- represents no failures
			trace ("all formulae in theory hold for current model") $
			trace ("returning model " ++ showModel model) $
			[model]

findFirstFailure :: Model -> [Formula] -> Maybe [Model]
-- 
findFirstFailure model [] = Nothing -- no failure found
findFirstFailure model@(domain,relations) (f:ormulae) =
	let self = findFirstFailure model in
	let bindings = allBindings (freeVariables f) domain [] in
	trace ("  checking formula: (v:" ++ show (length$variables f) ++ ") (fv:" ++ show (length$freeVariables f) ++ ") " ++ show f) $
	if holds model (UniversalQuantifier (freeVariables f) f) then self ormulae
	else Just $ findFirstBindingFailure model f bindings

findFirstBindingFailure :: Model -> Formula -> [Environment] -> [Model]
-- 
findFirstBindingFailure _ (Implication a Contradiction) _ = []
findFirstBindingFailure model formula@(Implication a b) (e:es) =
	let self = findFirstBindingFailure model formula in
	if holds' model e formula then self es
	else
		trace ("  attempting to satisfy (" ++ show b ++ ") with env " ++ show e) $
		satisfy model e b

satisfy :: Model -> Environment -> Formula -> [Model]
-- 
satisfy model env formula =
	let (domain,relations) = model in
	let domainSize = length domain in
	let self = satisfy model in
	case formula of
		Tautology -> [model]
		Contradiction -> []
		Or a b -> union (self env a) (self env b)
		And a b -> concatMap (\m -> satisfy m env b) (self env a)
		Equality v1 v2 -> case (lookup v1 env,lookup v2 env) of
			(Just v1, Just v2) -> [quotient model v1 v2]
			_ -> error("Could not look up one of \"" ++ show v2 ++ "\" or \"" ++ show v2 ++ "\" in environment")
		Atomic predicate vars ->
			let newRelationArgs = genNewRelationArgs env vars (fromIntegral (length domain)) in
			let newRelation = mkRelation predicate (length vars) [newRelationArgs] in
			let newModel = mkModel (mkDomain domainSize) (mergeRelation newRelation relations) in
			trace ("    adding new relation: " ++ show newRelation) $
			[newModel]
		ExistentialQuantifier [] f -> self env f
		ExistentialQuantifier (v:vs) f ->
			let f' = ExistentialQuantifier vs f in
			let nextDomainMember = fromIntegral $ (length domain) + 1 in
			if (domain /= []) && (any (\d -> holds' model (hashSet env v d) f') domain) then
				trace ("    " ++ show formula ++ " already holds") $
				[model]
			else
				trace ("    adding new domain element " ++ show nextDomainMember ++ " for variable " ++ (show v)) $
				satisfy (mkDomain nextDomainMember,relations) (hashSet env v nextDomainMember) f'
		_ -> error ("formula not in positive existential form: " ++ show formula)

genNewRelationArgs :: Environment -> [Variable] -> DomainMember -> [DomainMember]
-- for each Variable in the given list of Variables, retrieves the value
-- assigned to it in the given environment, or the next domain element if it
-- does not exist
genNewRelationArgs env [] domainSize = []
genNewRelationArgs env (v:vs) domainSize =
	let self = genNewRelationArgs env vs in
	case lookup v env of
		Just v' -> v' : self domainSize
		_ -> domainSize+1 : self (domainSize+1)
