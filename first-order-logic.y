{
module Main where
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
	: /* nothing */                         { Program [] }
	| formulaList optNEWLINE                { Program $1 }

formulaList
	: formula                               { [$1] }
	| formula NEWLINE formulaList           { $1 : $3 }

formula
	: expr                                      { $1 }
	| expr IMPLIES expr                         { Implication $1 $3 }
	| FOR_ALL index quantifierBody              { UniversalQuantifier $2 $3 }
	| THERE_EXISTS_ONE index quantifierBody     { ExistentialQuantifier true $2 $3 }
	| THERE_EXISTS index quantifierBody         { ExistentialQuantifier false $2 $3 }

expr: exprOR { $1 }

exprOR
	: exprAND                               { $1 }
	| exprOR OR exprOR                      { ExpressionOR $1 $3 }

exprAND
	: exprValue                             { $1 }
	| exprAND AND exprAND                   { ExpressionAND $1 $3 }

exprValue
	: predicate                             { $1 }
	| PAREN_OPEN formula PAREN_CLOSE        { ParentheticalExpresson $2 }
	| TAUTOLOGY                             { Tautology }
	| CONTRADICTION                         { Contradiction }
	| NOT exprValue                         { Not $2 }

quantifierBody
	: optCOLON formula                      { $2 }
	| BRACE_OPEN formula BRACE_CLOSE        { $2 }

predicate: IDENTIFIER index                 { Predicate $1 $2 } ;

index
	: BRACKET_OPEN argList BRACKET_CLOSE    { $2 }
	| argListValue                          { $1 }

argList
	: argListValue                          { [$1] }
	| argListValue COMMA argList            { $1 : $3 }

argListValue
	: predicate                             { $1 }
	| FREE_VARIABLE                         { $1 }
	| INTEGER                               { $1 }

optCOLON:   /* nothing */ { false } | COLON   { $1 } ;
optNEWLINE: /* nothing */ { false } | NEWLINE { $1 } ;

