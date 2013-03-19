%{
(*
 * We really shouldn't have to require Me here
open Me
include Me
type 'a tree = {l: 'a; c: 'a tree list}
(*	| LPAREN ifix RPAREN	{ $2 } *)

	| ifix BINARY ifix	{ {l= $2; c=[$1; $3]} }
        | ifix PREFIX ifix	{ {l= $2; c=[$1; $3]} } 
	| LPAREN ifix BINARY ifix RPAREN	{ {l= $3; c=[$2; $4]} }
	| LPAREN ifix PREFIX ifix RPAREN	{ {l= $3; c=[$2; $4]} }
	| LPAREN ifixsi RPAREN	{ $2 }

	| ifixsi PREFIX ifixsi	{ {l= $2; c=[$3; $1]} } 
        ifixsi 			{ $1 }
	| UNI ifix		{ {l= $1; c=[$2]} }
	|  ifixsi BINARY ifixsi 	{ {l= $2; c=[$1; $3]} }
	|  ifixsi PREFIX ifixsi 	{ {l= $2; c=[$1; $3]} }
*)
open Me
%}


%token  <string> EQUALS
%token LPAREN RPAREN
%token UNTIL SINCE EOF
%token SEMICOLON COMMA
%token <string> UNI
%token <string> ATOM
%token <string> BINARY
%token <string> PREFIX 
%token <string> BINARY

%left EQUALS
%left COMMA
%left SEMICOLON
%left BINARY UNTIL SINCE
%left PREFIX
%right UNI

%start formula ifixs
%type <string Me.tree> formula
%type <string Me.tree> ifixs

%%
formula: phi EOF		{ $1 }	
	 | ifix EOF		{ $1 }
;
phi:  ATOM			{ {l= $1; c=[]} }
	| LPAREN phi RPAREN	{ $2 }
	| UNI phi		{ {l= $1; c=[$2]} }
	| phi BINARY phi	{ {l= $2; c=[$1; $3]} }
	| PREFIX LPAREN phi COMMA phi RPAREN{ {l= $1; c=[$3;$5]} }
	| PREFIX phi COMMA phi { {l= $1; c=[$2;$4]} }
	| PREFIX phi phi { {l= $1; c=[$2;$3]} }
;
ifix:  ATOM			{ {l= $1; c=[]} }
	| LPAREN ifix RPAREN	{ $2 }
	| UNI ifix		{ {l= $1; c=[$2]} }
	| ifix BINARY ifix	{ {l= $2; c=[$1; $3]} }
	| ifix PREFIX ifix	{ {l= $2; c=[$3; $1]} } 
        /* perhaps it would be better to define our data structures such
         * that c=[$1;$3] for ifix "PREFIX" operators and instead have
         * c=[$3;$1] for phi "PREFIX" operators
         */
;

ifixsi:  ATOM			{ {l= $1; c=[]} }
	| UNI ifixsi		{ {l= $1; c=[$2]} }
	| LPAREN ifixsi EQUALS ifixsi RPAREN	{ {l= $3; c=[$2; $4]} }
	| LPAREN ifixsi BINARY ifixsi RPAREN	{ {l= $3; c=[$2; $4]} }
	| LPAREN ifixsi PREFIX ifixsi RPAREN	{ {l= $3; c=[$2; $4]} }
	|  ifixsi EQUALS ifixsi 	{ {l= $2; c=[$1; $3]} }
	|  ifixsi BINARY ifixsi 	{ {l= $2; c=[$1; $3]} }
	|  ifixsi PREFIX ifixsi 	{ {l= $2; c=[$1; $3]} }
        /* perhaps it would be better to define our data structures such
         * that c=[$1;$3] for ifix "PREFIX" operators and instead have
         * c=[$3;$1] for phi "PREFIX" operators
         */
;
ifixs:  
        ifixsi 			{ $1 }
        /* perhaps it would be better to define our data structures such
         * that c=[$1;$3] for ifix "PREFIX" operators and instead have
         * c=[$3;$1] for phi "PREFIX" operators
         */
;
%%
