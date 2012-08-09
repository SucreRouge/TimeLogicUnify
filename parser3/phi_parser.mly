%{
open Me
include Me
%}

%token LPAREN RPAREN
%token UNTIL SINCE EOF
%token SEMICOLON NEG COMMA
%token <string> ATOM
%token <string> BINARY

%left UNTIL SINCE AND NEG COMMA 

%start formula
%type <string Me.tree> formula

%%
formula: phi EOF			{ $1 }
phi:  ATOM			{ {l= $1; c=[]} }
	| NEG phi		{ {l= "-"; c=[$2]} }
	| phi BINARY phi	{ {l= $2 ; c=[$1; $3]} }
	| UNTIL LPAREN phi COMMA phi RPAREN		{ {l= "U"; c=[$3;$5]} }
	| SINCE LPAREN phi COMMA phi RPAREN		{ {l= "S"; c=[$3;$5]} }
	| LPAREN phi RPAREN	{ $2 }
;
%%

