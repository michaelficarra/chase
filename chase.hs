module Chase where
import Parser
import Helpers
import Debug.Trace
import Data.List

chaseVerify :: [Formula] -> [Formula]
-- verifies that each formula is in positive existential form and performs some
-- normilization on implied/constant implications
chaseVerify formulae =
	let isNotPEF = not.isPEF in
	map (\f -> case f of
		Implication a b ->
			if isNotPEF a || isNotPEF b then error ("implication must be in positive existential form: " ++ showFormula f)
			else f
		_ ->
			if isNotPEF f then error ("formula must be in positive existential form: " ++ showFormula f)
			else (Implication Tautology f)
	) formulae

chase :: [Formula] -> [Model]
-- runs the chase algorithm on a given theory and returns a list of models that
-- satisfy it
chase formulae = chase' (chaseVerify formulae) ([],[(mkModel [] [])])

chase' :: [Formula] -> ([Model],[Model]) -> [Model]
-- used by the chase function to hide the model identity argument
chase' formulae (done,[]) = done
chase' formulae (done,pending) =
	let self = chase' formulae in
	let (p:ending) = pending in
	trace ("running chase on " ++ show (done,pending)) $
	if all (\f -> holds p (UniversalQuantifier (freeVariables f) f)) formulae then
		trace ("  all formulae in theory hold for model " ++ showModel p) $
		trace ("  moving model into done list") $
		self (union done [p],ending)
	else
		let possiblySatisfiedModels = attemptToSatisfyFirstFailure p formulae in
		trace ("  at least one formula does not hold for model " ++ showModel p) $
		trace ("  unioning " ++ show ending ++ " with [" ++ intercalate ", " (map showModel possiblySatisfiedModels) ++ "]") $
		self (done, union ending possiblySatisfiedModels)

attemptToSatisfyFirstFailure :: Model -> [Formula] -> [Model]
-- checks if each formula holds, sequentially, until one does not, then tries
-- to satisfy that formula
attemptToSatisfyFirstFailure model (f:ormulae) =
	let self = attemptToSatisfyFirstFailure model in
	if holds model (UniversalQuantifier (freeVariables f) f) then self ormulae
	else attemptToSatisfy model f

attemptToSatisfy :: Model -> Formula -> [Model]
-- returns a model that is altered so that the given formula will hold
attemptToSatisfy model formula =
	let f' = UniversalQuantifier (freeVariables formula) formula in
	trace ("  attempting to satisfy (" ++ showFormula formula ++ ")") $
	attemptToSatisfy' model [] f'

attemptToSatisfy' :: Model -> Environment -> Formula -> [Model]
-- hides the environment identity in the `attemptToSatisfy` function arguments
attemptToSatisfy' model env formula =
	let (domain,relations) = model in
	let domainSize = length domain in
	let self = attemptToSatisfy' model in
	-- trace ("  attempting to satisfy (" ++ showFormula formula ++ ") with env " ++ show env) $
	case formula of
		Tautology -> [model]
		Contradiction -> []
		Or a b -> union (self env a) (self env b)
		And a b -> concatMap (\m -> attemptToSatisfy' m env b) (self env a)
		Implication a b -> if holds' model env a then self env b else []
		Atomic predicate vars ->
			let newRelation = mkRelation predicate (length vars) [genNewRelationArgs env vars (fromIntegral (length domain))] in
			let newModel = mkModel (mkDomain domainSize) (mergeRelation newRelation relations) in
			trace ("    adding new relation: " ++ show newRelation) $
			[newModel]
		ExistentialQuantifier [] f -> self env f
		ExistentialQuantifier (v:vs) f ->
			let f' = ExistentialQuantifier vs f in
			let nextDomainElement = fromIntegral $ (length domain) + 1 in
			if any (\v' -> holds' model (hashSet env v v') f') domain then
				trace ("    " ++ showFormula formula ++ " already holds") $
				[model]
			else
				trace ("    adding new domain element " ++ show nextDomainElement ++ " for variable " ++ (show$variableName v)) $
				attemptToSatisfy' (mkDomain nextDomainElement,relations) (hashSet env v nextDomainElement) f'
		UniversalQuantifier [] f -> self env f
		UniversalQuantifier (v:vs) f ->
			let f' = UniversalQuantifier vs f in
			concatMap (\v' -> self (hashSet env v v') f') domain
		_ -> error ("formula not in positive existential form: " ++ showFormula formula)

genNewRelationArgs :: Environment -> [Variable] -> DomainElement -> [DomainElement]
-- for each Variable in the given list of Variables, retrieves the value
-- assigned to it in the given environment, or the next domain element if it
-- does not exist
genNewRelationArgs env [] domainSize = []
genNewRelationArgs env (v:ars) domainSize =
	let self = genNewRelationArgs env in
	case lookup v env of
		Just v' -> v' : (self ars domainSize)
		_ -> (domainSize+1) : (self ars (domainSize+1))