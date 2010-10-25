module Chase where
import Parser
import Helpers
import Data.List

verify :: Formula -> Formula
-- verifies that a formula is in positive existential form and performs some
-- normalization on implied/constant implications
verify formula = case formula of
	Implication a b -> Implication (pef a) (pef b)
	Not f -> Implication (pef f) Contradiction
	_ -> Implication Tautology (pef formula)

order :: [Formula] -> [Formula]
-- sorts a theory by number of disjuncts then by number of variables
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
-- manipulation / normalization
chase theory = nub $ chase' (order $ map verify theory) [([],[])]

chase' :: [Formula] -> [Model] -> [Model]
-- runs the chase algorithm on a given theory, manipulating the given list of
-- models, and returning a list of models that satisfy the theory
chase' _ [] = []
chase' theory pending = concatMap (branch theory) pending

branch :: [Formula] -> Model -> [Model]
-- if there is a (formula,environment) pairing for which the given model does not hold,
--   alters the model in attempt to make it hold
-- otherwise,
--   returns the given model
branch theory model =
	let reBranch = chase' theory in
	-- run chase on `model`
	case findFirstFailure model theory of
		Just newModels ->
			-- at least one formula does not hold for `model`
			reBranch newModels
		Nothing -> -- represents no failures
			-- all formulae in theory hold for current model, return it
			[model]

findFirstFailure :: Model -> [Formula] -> Maybe [Model]
-- test if there exists a (Formula,Environment) pairing for which the given
-- model does not hold
findFirstFailure model [] = Nothing -- no failure found
findFirstFailure model@(domain,relations) (f:ormulae) =
	let self = findFirstFailure model in
	let bindings = allBindings (freeVariables f) domain [] in
	-- check formula `f`
	if holds model (UniversalQuantifier (freeVariables f) f) then self ormulae
	else Just $ findFirstBindingFailure model f bindings

findFirstBindingFailure :: Model -> Formula -> [Environment] -> [Model]
-- tests if there is an environment for which the given model does not satisfy
-- the given formula
findFirstBindingFailure _ (Implication a Contradiction) _ = []
findFirstBindingFailure model formula@(Implication a b) (e:es) =
	let self = findFirstBindingFailure model formula in
	if holds' model e formula then self es
	else
		-- attempt to satisfy RHS `b` with binding `e`
		satisfy model e b

satisfy :: Model -> Environment -> Formula -> [Model]
-- alter the given model to satisfy the given formula under the given environment
satisfy model env formula =
	let (domain,relations) = model in
	let domainSize = fromIntegral $ length domain in
	let self = satisfy model in
	case formula of
		Tautology -> [model]
		Contradiction -> []
		Or a b -> union (self env a) (self env b)
		And a b -> concatMap (\m -> satisfy m env b) (self env a)
		Equality v1 v2 -> case (lookup v1 env,lookup v2 env) of
			(Just v1, Just v2) -> [quotient model v1 v2]
			_ -> error("environment lookup error")
		Atomic predicate vars ->
			let newRelationArgs = genNewRelationArgs env vars domainSize in
			let newRelation = mkRelation predicate (length vars) [newRelationArgs] in
			let newDomain = mkDomain domainSize in
			let newRelations = mergeRelation newRelation relations in
			let newModel = mkModel newDomain newRelations in
			-- add new relation `newRelation` to `model`
			[newModel]
		ExistentialQuantifier [] f -> self env f
		ExistentialQuantifier (v:vs) f ->
			let f' = ExistentialQuantifier vs f in
			let modelHoldsIn = \d -> holds' model (hashSet env v d) f' in
			let nextDomainMember = fromIntegral $ (length domain) + 1 in
			let newModel = mkModel (mkDomain nextDomainMember) relations in
			let newEnvironment = hashSet env v nextDomainMember in
			if (domain /= []) && any modelHoldsIn domain then
				-- `formula` already holds
				[model]
			else
				-- add new domain member `nextDomainMember` for variable `v`
				-- and expand domain of model to `nextDomainMember` in length
				satisfy newModel newEnvironment f'
		_ -> error ("formula not in positive existential form: " ++ show formula)

genNewRelationArgs :: Environment -> [Variable] -> DomainMember -> [DomainMember]
-- for each variable in the given list of variables, retrieves the value
-- assigned to it in the given environment, or the next domain element if it
-- does not exist
genNewRelationArgs env [] domainSize = []
genNewRelationArgs env (v:vs) domainSize =
	let self = genNewRelationArgs env vs in
	case lookup v env of
		Just v' -> v' : self domainSize
		_ -> domainSize+1 : self (domainSize+1)
