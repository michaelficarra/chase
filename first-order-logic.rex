class FirstOrderLogic
rule
	[\r\n]+                     {[:NEWLINE, text]}
	\s+                         { next_token } # ignore whitespace
	\/\*(?:[^\*]|\*[^\/])*\*\/  { next_token } # ignore comments
	facts                       {[:FACTS, text]}
	rules                       {[:RULES, text]}
	chase                       {[:CHASE, text]}
	[A-Z]+[a-z]+                {
	                                match = text.match(/^([A-Z]+)([a-z]+)$/);
	                                [:PREDICATE, {
	                                    :symbol => match[1],
	                                    :terminals => match[2].split('')
	                                }]
	                            }
	⊤                           {[:TAUTOLOGY, text]}
	⊥                           {[:CONTRADICTION, text]}
	∃!?[a-z]+                   {
	                                symbol = (text =~ /!/ ? :THERE_EXISTS_ONE : :THERE_EXISTS);
	                                [symbol, text.match(/([a-z]+)/)[1].split('')]
	                            }
	\d+                         {[:INTEGER, text.to_i]}
	∀[a-z]+                     {[:FOR_ALL, text.match(/∀([a-z]+)/)[1].split('')]}
	[a-z][a-zA-Z0-9_-]*         {[:IDENTIFIER, text]}
	\{\s*\}|∅                   {[:EMPTY_SET, text]}
	->|→|⇒|⊃                    {[:IMPLIES, text]}
	\|\||∨                      {[:OR, text]}
	&&|∧                        {[:AND, text]}
	⊕                           {[:XOR, text]}
	=                           {[:EQ, text]}
	¬=|≠                        {[:NE, text]}
	>=|≤                        {[:LTE, text]}
	<=|≥                        {[:GTE, text]}
	<                           {[:LT, text]}
	>                           {[:GT, text]}
	\+                          {[:PLUS, text]}
	\-                          {[:MINUS, text]}
	¬                           {[:NOT, text]}
	[\*·]                       {[:MULTIPLY, text]}
	[\/÷]                       {[:DIVIDE, text]}
	\^                          {[:EXP, text]}
	\(                          {[:PAREN_OPEN, text]}
	\)                          {[:PAREN_CLOSE, text]}
	\{                          {[:BRACE_OPEN, text]}
	\}                          {[:BRACE_CLOSE, text]}
	,                           {[:COMMA, text]}
	:                           {[:COLON, text]}
	;                           {[:SEMICOLON, text]}
end
