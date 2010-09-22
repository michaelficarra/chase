{
module Parser where
import Lexer
import Word
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
	| FOR_ALL argList quantifierBody        { UniversalQuantifier $2 $3 }
	| THERE_EXISTS argList quantifierBody   { ExistentialQuantifier $2 $3 }

quantifierBody: optCOLON formula            { $2 }

expr
	: exprOR                                { $1 }
	| exprOR "->" formula                   { Implication $1 $3 }
	| "->" formula                          { Implication Tautology $2 }

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
decomposeImplication (Implication a b) = (a,b)
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

mkDomain :: Integral i => i -> Domain
mkDomain size = [1..(fromIntegral size)]
mkRelation :: Integral a => String -> a -> [[DomainElement]] -> Relation
mkRelation name arity truthTable = (name,(fromIntegral arity),truthTable)
mkModel domain relations = (domain,relations)

parseError :: [Token] -> a
parseError tokenList =
	let pos = tokenPosn $ head tokenList in
	error ("parse error: unexpected " ++ (showToken$head tokenList) ++ " at line " ++ (show$getLineNum pos) ++ ", column " ++ (show$getColumnNum pos))

}
