{
module Main (eval,main) where
}

%name generate
%tokentype { Token }
%error { parseError }

%token
	OR                 { TokenOR }
	AND                { TokenAND }
	NOT                { TokenNOT }
	BRACKET_OPEN       { TokenBracketOpen }
	BRACKET_CLOSE      { TokenBracketClose }
	PAREN_OPEN         { TokenParenOpen }
	PAREN_CLOSE        { TokenParenClose }
	TAUTOLOGY          { TokenTautology }
	CONTRADICTION      { TokenContradiction }
	FOR_ALL            { TokenForAll }
	THERE_EXISTS       { TokenThereExists }
	COLON              { TokenColon }
	COMMA              { TokenComma }
	IMPLIES            { TokenImplies }
	FREE_VARIABLE      { TokenFreeVariable }
	INTEGER            { TokenInteger }
	PREDICATE          { TokenPredicate }
	NEWLINE            { TokenNewline }

%%

program
	:                                       { Main.Nothing }
	| formulaList optNEWLINE                { $1 }

formulaList
	: formula                               { [(Formula $1)] }
	| formulaList NEWLINE formula           { $1 ++ [(Formula $3)] }

formula
	: expr                                      { Expression $1 }
	| expr IMPLIES expr                         { Implication $1 $3 }
	| FOR_ALL argList quantifierBody            { UniversalQuantifier $2 $3 }
	| THERE_EXISTS argList quantifierBody       { ExistentialQuantifier False $2 $3 }

expr: exprOR { ExpressionOR $1 }

exprOR
	: exprAND                               { ExpressionAND $1 }
	| exprOR OR exprAND                     { ExpressionOR $1 $3 }

exprAND
	: exprValue                             { ExpressionValue $1 }
	| exprAND AND exprValue                 { ExpressionAND $1 $3 }

exprValue
	: atomic                                { PredicateEV $1 }
	| PAREN_OPEN formula PAREN_CLOSE        { ParentheticalExpression (Formula $2) }
	| BRACKET_OPEN formula BRACKET_CLOSE    { ParentheticalExpression (Formula $2) }
	| TAUTOLOGY                             { $1 }
	| CONTRADICTION                         { $1 }
	| NOT exprValue                         { Not $2 }

quantifierBody: optCOLON formula            { Formula $2 }

atomic: PREDICATE index                     { Predicate $1 (Index $2) }

index
	: PAREN_OPEN argList PAREN_CLOSE        { $2 }
	| BRACKET_OPEN argList BRACKET_CLOSE    { $2 }

argList
	: arg                                   { [(Arg $1)] }
	| argList COMMA arg                     { $1 ++ [(Arg $3)] }

arg
	: atomic                                { PredicateArg $1 }
	| FREE_VARIABLE                         { FreeVariable $1 }
	| INTEGER                               { Integer $1 }

optCOLON:   { Main.Nothing } | COLON   { $1 }
optNEWLINE: { Main.Nothing } | NEWLINE { $1 }

{
eval = do
	s <- readFile "./simple-grammar-sample.fol"
	putStr s
	print (alexScanTokens s)

data Program
	= Nothing
	| FormulaList
	deriving Show

type FormulaList = [Formula]

data Formula
	= Expression
	| Implication Expression Expression
	| UniversalQuantifier Index QuantifierBody
	| ExistentialQuantifier Bool Index QuantifierBody
	deriving Show

data Expression
	= ExpressionOR ExpressionOR
	deriving Show

data ExpressionOR
	= ExpressionAND ExpressionAND
	deriving Show

data ExpressionAND
	= ExpressionValue ExpressionValue
	deriving Show

data ExpressionValue
	= PredicateEV Predicate
	| ParentheticalExpression Formula
	| Tautology
	| Contradiction
	| Not ExpressionValue
	deriving Show

type Tautology = String
type Contradiction = String

data Predicate
	= Predicate String Index
	deriving Show

data QuantifierBody
	= Formula Formula
	deriving Show

data Index
	= ArgList
	deriving Show

type ArgList = [Arg]

data Arg
	= PredicateArg Predicate
	| FreeVariable String
	| Integer String
	deriving Show

data Token
	= TokenOR
	| TokenAND
	| TokenNOT
	| TokenBracketOpen
	| TokenBracketClose
	| TokenBraceOpen
	| TokenBraceClose
	| TokenParenOpen
	| TokenParenClose
	| TokenTautology
	| TokenContradiction
	| TokenForAll
	| TokenThereExists
	| TokenThereExistsOne
	| TokenColon
	| TokenComma
	| TokenImplies
	| TokenFreeVariable
	| TokenInteger
	| TokenIdentifier
	| TokenNewline
	deriving Show

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
