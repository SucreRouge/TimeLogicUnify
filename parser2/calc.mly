/* file: calc.mly */
/* Infix notatoin calculator -- calc */
%{
open Printf
open Mechecker
%}
/*
*/

/* Ocamlyacc Declarations */
%token NEWLINE
%token LPAREN RPAREN
%token UNTIL SINCE AND NEG COMMA OR
%token <char> ATOM

%left UNTIL SINCE AND NEG

%start input
%type <unit> input

/* Grammar follows */
%%
input:	/* empty */	{ }
	| input line	{ }
;
line:	NEWLINE		{ }
	| exp NEWLINE   { Printf.printf "%s\n" (string_from_ftree $1); flush stdout } 
exp:	ATOM			{ {op= $1; ch=[]} }
	| exp AND exp		{ {op= '^'; ch=[$1; $3]} }
	| exp OR exp		{ {op= '|'; ch=[$1; $3]} }
	| NEG exp		{ {op= '-'; ch=[$2]} }
	| UNTIL LPAREN exp COMMA exp RPAREN		{ {op= 'U'; ch=[$3;$5]} }
	| SINCE LPAREN exp COMMA exp RPAREN		{ {op= 'S'; ch=[$3;$5]} }
	| LPAREN exp RPAREN	{ $2 }

me 
;

%%

