{
module Parser (eval,main) where
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
	BRACE_OPEN         { TokenBraceOpen }
	BRACE_CLOSE        { TokenBraceClose }
	PAREN_OPEN         { TokenParenOpen }
	PAREN_CLOSE        { TokenParenClose }
	TAUTOLOGY          { TokenTautology }
	CONTRADICTION      { TokenContradiction }
	FOR_ALL            { TokenForAll }
	THERE_EXISTS       { TokenThereExists }
	THERE_EXISTS_ONE   { TokenThereExistsOne }
	COLON              { TokenColon }
	COMMA              { TokenComma }
	IMPLIES            { TokenImplies }
	FREE_VARIABLE      { TokenFreeVariable }
	INTEGER            { TokenInteger }
	IDENTIFIER         { TokenIdentifier }
	NEWLINE            { TokenNewline }

%%

program
	:                                       { Nothing }
	| formulaList optNEWLINE                { $1 }

formulaList
	: formula                               { [(Formula $1)] }
	| formulaList NEWLINE formula           { $1 ++ [(Formula $3)] }

formula
	: expr                                      { Expression $1 }
	| expr IMPLIES expr                         { Implication $1 $3 }
	| FOR_ALL index quantifierBody              { UniversalQuantifier $2 $3 }
	| THERE_EXISTS_ONE index quantifierBody     { ExistentialQuantifier true $2 $3 }
	| THERE_EXISTS index quantifierBody         { ExistentialQuantifier false $2 $3 }

expr: exprOR { ExpressionOR $1 }

exprOR
	: exprAND                               { ExpressionAND $1 }
	| exprOR OR exprAND                     { ExpressionOR $1 $3 }

exprAND
	: exprValue                             { ExpressionValue $1 }
	| exprAND AND exprValue                 { ExpressionAND $1 $3 }

exprValue
	: predicate                             { $1 }
	| PAREN_OPEN formula PAREN_CLOSE        { ParentheticalExpresson (Formula $2) }
	| TAUTOLOGY                             { $1 }
	| CONTRADICTION                         { $1 }
	| NOT exprValue                         { Not $2 }

quantifierBody
	: optCOLON formula                      { Formula $2 }
	| BRACE_OPEN formula BRACE_CLOSE        { Formula $2 }

predicate: IDENTIFIER index                 { Predicate $1 (Index $2) }

index
	: BRACKET_OPEN argList BRACKET_CLOSE    { $2 }
	| arg                                   { [(Arg $1)] }

argList
	: arg                                   { [(Arg $1)] }
	| argList COMMA arg                     { $1 ++ [(Arg $3)] }

arg
	: predicate                             { $1 }
	| FREE_VARIABLE                         { FreeVariable $1 }
	| INTEGER                               { Integer $1 }

optCOLON:   { false } | COLON   { $1 }
optNEWLINE: { false } | NEWLINE { $1 }

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
	= ExpressionOR
	deriving Show

data ExpressionOR
	= ExpressionAND ExpressionAND
	| ExpressionOR ExpressionOR ExpressionOR
	deriving Show

data ExpressionAND
	= ExpressionValue ExpressionValue
	| ExpressionAND ExpressionAND ExpressionAND
	deriving Show

data ExpressionValue
	= Predicate String Index
	| ParentheticalExpression Formula
	| Tautology
	| Contradiction
	| Not ExpressionValue
	deriving Show

type Tautology = TokenTautology
type Contradiction = TokenContradiction

data QuantifierBody
	= Formula Formula
	deriving Show

data Index
	= ArgList
	deriving Show

type ArgList = [Arg]

data Arg
	= Predicate String Index
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
