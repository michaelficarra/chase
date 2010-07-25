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
	∃[a-z]+                     {[:THERE_EXISTS, text.match(/∃([a-z]+)/)[1].split('')]}
	∀[a-z]+                     {[:FOR_ALL, text.match(/∀([a-z]+)/)[1].split('')]}
	[a-z][a-zA-Z0-9_-]*         {[:IDENTIFIER, text]}
	\{\s*\}|∅                   {[:EMPTY_SET, text]}
	->|→|⇒|⊃                    {[:IMPLIES, text]}
	\|\||∨                      {[:OR, text]}
	&&|∧                        {[:AND, text]}
	==?                         {[:EQ, text]}
	¬=|≠                        {[:NE, text]}
	>=|≤                        {[:LTE, text]}
	<=|≥                        {[:GTE, text]}
	<                           {[:LT, text]}
	>                           {[:GT, text]}
	\+                          {[:PLUS, text]}
	\-                          {[:MINUS, text]}
	[\*·]                       {[:MULTIPLY, text]}
	[\/÷]                       {[:DIVIDE, text]}
	¬                           {[:NOT, text]}
	\(                          {[:PAREN_OPEN, text]}
	\)                          {[:PAREN_CLOSE, text]}
	\{                          {[:BRACE_OPEN, text]}
	\}                          {[:BRACE_CLOSE, text]}
	,                           {[:COMMA, text]}
	:                           {[:COLON, text]}
	;                           {[:SEMICOLON, text]}
end
