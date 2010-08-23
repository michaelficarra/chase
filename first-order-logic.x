{
	module Main (alexScanTokens, printTokens) where
}

%wrapper "basic"

$a = [Aa]
$b = [Bb]
$c = [Cc]
$d = [Dd]
$e = [Ee]
$f = [Ff]
$g = [Gg]
$h = [Hh]
$i = [Ii]
$j = [Jj]
$k = [Kk]
$l = [Ll]
$m = [Mm]
$n = [Nn]
$o = [Oo]
$p = [Pp]
$q = [Qq]
$r = [Rr]
$s = [Ss]
$t = [Tt]
$u = [Uu]
$v = [Vv]
$w = [Ww]
$x = [Xx]
$y = [Yy]
$z = [Zz]

tokens :-

	[\r\n]+                         { \s -> ("NEWLINE",s) }
	[\ \t]+                         ; -- ignore whitespace
	"-- "[^\r\n]*                   ; -- ignore comments
	"#"[^\r\n]*                     ; -- ignore comments
	"/*"([^\*]|"*"[^\/])*"*/"       ; -- ignore comments
	$t$h$e$r$e$e$x$i$s$t$s$o$n$e    { \s -> ("THERE_EXISTS_ONE",s) }
	$t$h$e$r$e$e$x$i$s$t$s          { \s -> ("THERE_EXISTS",s) }
	$f$o$r$a$l$l                    { \s -> ("FOR_ALL",s) }
	$t$a$u$t$o$l$o$g$y              { \s -> ("TAUTOLOGY",s) }
	$c$o$n$t$r$a$d$i$c$t$i$o$n      { \s -> ("CONTRADICTION",s) }
	"->"|"=>"                       { \s -> ("IMPLIES",s) }
	$n$o$t|"!"                      { \s -> ("NOT",s) }
	$o$r|"|"                        { \s -> ("OR",s) }
	$a$n$d|"&"                      { \s -> ("AND",s) }
	"("                             { \s -> ("PAREN_OPEN",s) }
	")"                             { \s -> ("PAREN_CLOSE",s) }
	"["                             { \s -> ("BRACKET_OPEN",s) }
	"]"                             { \s -> ("BRACKET_CLOSE",s) }
	"{"                             { \s -> ("BRACE_OPEN",s) }
	"}"                             { \s -> ("BRACE_CLOSE",s) }
	":"                             { \s -> ("COLON",s) }
	","                             { \s -> ("COMMA",s) }
	[a-z]+                          { \s -> ("TERMINAL",s) }
	[0-9]+                          { \s -> ("INTEGER",s) }
	[^\[a-z\ \t]+                   { \s -> ("IDENTIFIER",s) }

{
printTokens = do
	s <- readFile "./simple-grammar-sample.fol"
	-- putStr s
	print (alexScanTokens s)
}
