class FirstOrderLogic
rule
	[\r\n]+                     {[:NEWLINE, text]}
	[#{32.chr}\t]+              { next_token } # ignore whitespace
	(?:--\s+|\#)[^\r\n]*        { next_token } # ignore comments
	\/\*(?:[^\*]|\*[^\/])*\*\/  { next_token } # ignore comments
	∃!                          {[:THERE_EXISTS_ONE, text]}
	∃                           {[:THERE_EXISTS, text]}
	∀                           {[:FOR_ALL, text]}
	⊤                           {[:TAUTOLOGY, text]}
	⊥                           {[:CONTRADICTION, text]}
	=>|->|→|⇒|⊃                 {[:IMPLIES, text]}
	¬                           {[:NOT, text]}
	\||∨                        {[:OR, text]}
	&|∧                         {[:AND, text]}
	\(                          {[:PAREN_OPEN, text]}
	\)                          {[:PAREN_CLOSE, text]}
	\[                          {[:BRACKET_OPEN, text]}
	\]                          {[:BRACKET_CLOSE, text]}
	\{                          {[:BRACE_OPEN, text]}
	\}                          {[:BRACE_CLOSE, text]}
	:                           {[:COLON, text]}
	,                           {[:COMMA, text]}
	[a-z]+                      {[:FREE_VARIABLE, text]}
	[0-9]+                      {[:INTEGER, text.to_i]}
	[^\[a-z#{32.chr}\t]+        {[:IDENTIFIER, text]}
end
