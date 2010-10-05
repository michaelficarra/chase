class FirstOrderLogic
rule
	[\r\n]+                     {[:NEWLINE, text]}
	[#{32.chr}\t]+              { next_token } # ignore whitespace
	(?:--\s+|\#)[^\r\n]*        { next_token } # ignore comments
	\/\*(?:[^\*]|\*[^\/])*\*\/  { next_token } # ignore comments
	∃|Exists                    {[:THERE_EXISTS, text]}
	∀|For[Aa]ll                 {[:FOR_ALL, text]}
	⊤|[Tt]autology              {[:TAUTOLOGY, text]}
	⊥|[Cc]ontradiction          {[:CONTRADICTION, text]}
	=>|->|→|⇒|⊃                 {[:IMPLIES, text]}
	!|¬                         {[:NOT, text]}
	\||∨                        {[:OR, text]}
	&|∧                         {[:AND, text]}
	\(                          {[:PAREN_OPEN, text]}
	\)                          {[:PAREN_CLOSE, text]}
	\[                          {[:BRACKET_OPEN, text]}
	\]                          {[:BRACKET_CLOSE, text]}
	:                           {[:COLON, text]}
	,                           {[:COMMA, text]}
	[a-z][a-z0-9_']*            {[:FREE_VARIABLE, text]}
	[A-Z][a-z0-9_']*            {[:PREDICATE, text]}
end
