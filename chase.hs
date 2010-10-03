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
chase formulae = chase' (chaseVerify formulae) [mkModel [] []]

chase' :: [Formula] -> [Model] -> [Model]
-- used by the chase function to hide the model identity argument
chase' formulae [] = []
chase' formulae pending@(p:ending) =
	let self = chase' formulae in
	trace ("running chase on " ++ show pending) $
	case attemptToSatisfyFirstFailure p formulae of
		Just models ->
			trace ("  at least one formula does not hold for model " ++ showModel p) $
			trace ("  unioning " ++ show ending ++ " with [" ++ intercalate ", " (map showModel models) ++ "]") $
			self (union ending models)
		Nothing ->
			trace ("  all formulae in theory hold for model " ++ showModel p) $
			trace ("  moving model into done list") $
			p : self ending

attemptToSatisfyFirstFailure :: Model -> [Formula] -> Maybe [Model]
attemptToSatisfyFirstFailure model [] = Nothing
attemptToSatisfyFirstFailure model@(domain,relations) (f:ormulae) =
	let self = attemptToSatisfyFirstFailure model in
	let bindings = allBindings (freeVariables f) domain [] in
	if holds model (UniversalQuantifier (freeVariables f) f) then self ormulae
	else Just $ attemptToSatisfyFirstBindingFailure model f bindings

attemptToSatisfyFirstBindingFailure :: Model -> Formula -> [Environment] -> [Model]
attemptToSatisfyFirstBindingFailure model formula (e:es) =
	let self = attemptToSatisfyFirstBindingFailure model formula in
	if holds' model e formula then self es
	else
		trace ("  attempting to satisfy (" ++ showFormula formula ++ ") with env " ++ show e) $
		attemptToSatisfy model e formula

allBindings :: [Variable] -> Domain -> Environment -> [Environment]
allBindings [] _ env = [env]
allBindings (v:vs) domain env =
	concatMap (allBindings vs domain) (map (hashSet env v) domain)

attemptToSatisfy :: Model -> Environment -> Formula -> [Model]
-- hides the environment identity in the `attemptToSatisfy` function arguments
attemptToSatisfy model env formula =
	let (domain,relations) = model in
	let domainSize = length domain in
	let self = attemptToSatisfy model in
	case formula of
		Tautology -> [model]
		Contradiction -> []
		Or a b -> union (self env a) (self env b)
		And a b -> concatMap (\m -> attemptToSatisfy m env b) (self env a)
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
			if any (\v' -> holds' model (hashSet env v v') f') domain then
				trace ("    " ++ showFormula formula ++ " already holds") $
				[model]
			else
				trace ("    adding new domain element " ++ show nextDomainElement ++ " for variable " ++ (show$variableName v)) $
				attemptToSatisfy (mkDomain nextDomainElement,relations) (hashSet env v nextDomainElement) f'
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
