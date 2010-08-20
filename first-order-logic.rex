class FirstOrderLogic
rule
	[\r\n]+                     {[:NEWLINE, text]}
	\s+                         { next_token } # ignore whitespace
	(?:--\s+|#).*               { next_token } # ignore comments
	\/\*(?:[^\*]|\*[^\/])*\*\/  { next_token } # ignore comments
	∃!          {[:THERE_EXISTS_ONE, text]}
	∃           {[:THERE_EXISTS, text]}
	∀           {[:FOR_ALL, text]}
	⊤           {[:TAUTOLOGY, text]}
	⊥           {[:CONTRADICTION, text]}
	->|→|⇒|⊃    {[:IMPLIES, text]}
	¬           {[:NOT, text]}
	\||∨        {[:OR, text]}
	&|∧         {[:AND, text]}
	⊕           {[:XOR, text]}
	=           {[:EQ, text]}
	\(          {[:PAREN_OPEN, text]}
	\)          {[:PAREN_CLOSE, text]}
	\[          {[:BRACKET_OPEN, text]}
	\]          {[:BRACKET_CLOSE, text]}
	\{          {[:BRACE_OPEN, text]}
	\}          {[:BRACE_CLOSE, text]}
	:           {[:COLON, text]}
	,           {[:COMMA, text]}
	[a-z]       {[:TERMINAL, text]}
	\d+         {[:INTEGER, text.to_i]}
	[^\[]+      {[:IDENTIFIER, text]}
end
