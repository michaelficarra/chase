module Chase where
import Parser
import Helpers
import qualified Debug.Trace
import Data.List

trace x = id

chaseVerify :: Formula -> Formula
-- verifies that a formula is in positive existential form and performs some
-- normalization on implied/constant implications
chaseVerify formula =
	let isNotPEF = not.isPEF in
	case formula of
		Implication a b ->
			if isNotPEF a || isNotPEF b then
				error ("implication must be in positive existential form: " ++ showFormula formula)
			else formula
		_ ->
			if isNotPEF formula then
				error ("formula must be in positive existential form: " ++ showFormula formula)
			else (Implication Tautology formula)

chase :: [Formula] -> [Model]
-- a wrapper for the chase' function to hide the model identity and theory
-- manipulation
chase formulae = chase' (map chaseVerify formulae) [([],[])]

chase' :: [Formula] -> [Model] -> [Model]
-- runs the chase algorithm on a given theory, manipulating the given list of
-- models, and returning a list of models that satisfy the theory
chase' formulae [] = []
chase' formulae pending@(m:rest) =
	let self = chase' formulae in
	trace ("running chase on " ++ show pending) $
	case findFirstFailure m formulae of
		Just newPending ->
			trace ("  at least one formula does not hold for model " ++ showModel m) $
			trace ("  unioning " ++ show rest ++ " with [" ++ intercalate ", " (map showModel newPending) ++ "]") $
			self (union rest newPending)
		Nothing -> -- represents no failures
			trace ("  all formulae in theory hold for model " ++ showModel m) $
			trace ("  moving model into done list") $
			m : self rest

findFirstFailure :: Model -> [Formula] -> Maybe [Model]
-- 
findFirstFailure model [] = Nothing -- no failure found
findFirstFailure model@(domain,relations) (f:ormulae) =
	let self = findFirstFailure model in
	let bindings = allBindings (freeVariables f) domain [] in
	if holds model (UniversalQuantifier (freeVariables f) f) then self ormulae
	else Just $ findFirstBindingFailure model f bindings

findFirstBindingFailure :: Model -> Formula -> [Environment] -> [Model]
-- 
findFirstBindingFailure model formula (e:es) =
	let self = findFirstBindingFailure model formula in
	if holds' model e formula then self es
	else
		trace ("  attempting to satisfy (" ++ showFormula formula ++ ") with env " ++ show e) $
		chaseSatisfy model e formula

chaseSatisfy :: Model -> Environment -> Formula -> [Model]
-- 
chaseSatisfy model env formula =
	let (domain,relations) = model in
	let domainSize = length domain in
	let self = chaseSatisfy model in
	case formula of
		Tautology -> [model]
		Contradiction -> []
		Or a b -> union (self env a) (self env b)
		And a b -> concatMap (\m -> chaseSatisfy m env b) (self env a)
		Equality v1 v2 -> case (lookup v1 env,lookup v2 env) of
			(Just v1, Just v2) -> [quotient model v1 v2]
			_ -> error("Could not look up one of \"" ++ variableName v2 ++ "\" or \"" ++ variableName v2 ++ "\" in environment")
		Implication a b -> if holds' model env a then self env b else []
		Atomic predicate vars ->
			let newRelationArgs = genNewRelationArgs env vars (fromIntegral (length domain)) in
			let newRelation = mkRelation predicate (length vars) [newRelationArgs] in
			let newModel = mkModel (mkDomain domainSize) (mergeRelation newRelation relations) in
			trace ("    adding new relation: " ++ show newRelation) $
			[newModel]
		ExistentialQuantifier [] f -> self env f
		ExistentialQuantifier (v:vs) f ->
			let f' = ExistentialQuantifier vs f in
			let nextDomainElement = fromIntegral $ (length domain) + 1 in
			if (domain /= []) && (any (\v' -> holds' model (hashSet env v v') f') domain) then
				trace ("    " ++ showFormula formula ++ " already holds") $
				[model]
			else
				trace ("    adding new domain element " ++ show nextDomainElement ++ " for variable " ++ (show$variableName v)) $
				chaseSatisfy (mkDomain nextDomainElement,relations) (hashSet env v nextDomainElement) f'
		_ -> error ("formula not in positive existential form: " ++ showFormula formula)

genNewRelationArgs :: Environment -> [Variable] -> DomainElement -> [DomainElement]
-- for each Variable in the given list of Variables, retrieves the value
-- assigned to it in the given environment, or the next domain element if it
-- does not exist
genNewRelationArgs env [] domainSize = []
genNewRelationArgs env (v:vs) domainSize =
	let self = genNewRelationArgs env vs in
	case lookup v env of
		Just v' -> v' : self domainSize
		_ -> domainSize+1 : self (domainSize+1)
