%{
open Me
include Me
%}

%token LPAREN RPAREN
%token UNTIL SINCE EOF
%token SEMICOLON NEG COMMA
%token <string> ATOM
%token <string> BINARY
%token <string> PREFIX 

%left UNTIL SINCE AND NEG COMMA 

%start formula
%type <string Me.tree> formula

%%
formula: phi EOF		{ $1 }	
	 | ifix EOF		{ $1 }
;
phi:  ATOM			{ {l= $1; c=[]} }
	| NEG phi		{ {l= "-"; c=[$2]} }
	| phi BINARY phi	{ {l= $2 ; c=[$1; $3]} }
	| PREFIX LPAREN phi COMMA phi RPAREN{ {l= $1; c=[$3;$5]} }
	| PREFIX phi COMMA phi { {l= $1; c=[$2;$4]} }
	| PREFIX phi phi { {l= $1; c=[$2;$3]} }
	| LPAREN phi RPAREN	{ $2 }
;
ifix:  ATOM			{ {l= $1; c=[]} }
	| NEG ifix		{ {l= "-"; c=[$2]} }
	| ifix BINARY ifix	{ {l= $2 ; c=[$1; $3]} }
	| ifix PREFIX ifix	{ {l= $2 ; c=[$1; $3]} }
	| LPAREN ifix RPAREN	{ $2 }
;
%%

