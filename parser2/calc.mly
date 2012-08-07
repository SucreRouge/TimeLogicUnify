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
%token LBRACE RBRACE SEMICOLON LSQUARE RSQUARE GT LT IN PLUS
%token <string> ATOM

%left UNTIL SINCE AND NEG COMMA

%start input
%type <unit> input

/* Grammar follows */
%%
input:	/* empty */	{ }
	| input line	{ }
;
line:	NEWLINE		{ }
	| exp IN me NEWLINE   { ignore ( model_check $3 $1 ); flush stdout } 
/*	| exp IN me NEWLINE   { Printf.printf "%s\n" (string_from_ftree $1); flush stdout }  */
exp:	ATOM			{ {op= (String.get ($1) 0); ch=[]} }
	| exp AND exp		{ {op= '&'; ch=[$1; $3]} }
	| exp OR exp		{ {op= '|'; ch=[$1; $3]} }
	| NEG exp		{ {op= '-'; ch=[$2]} }
	| UNTIL LPAREN exp COMMA exp RPAREN		{ {op= 'U'; ch=[$3;$5]} }
	| SINCE LPAREN exp COMMA exp RPAREN		{ {op= 'S'; ch=[$3;$5]} }
	| LPAREN exp RPAREN	{ $2 }
;
shuflist:  /* empty */		{ [] }
	| me COMMA shuflist	{ $1::$3 }
	| me			{ [$1] }
;
atomlist:  /* empty */		{[]}
	| ATOM COMMA atomlist	{ $1::$3 }
	| ATOM			{ [$1] }
;
me:	LBRACE atomlist RBRACE	{ {l=LabelS(letter_of_list $2);c=[]} }
	| LSQUARE shuflist RSQUARE	{ {l=LabelC 'S';c=$2} }
	| me PLUS me		{ {l=LabelC '+'; c=[$1;$3] } }
	| GT me			{ {l=LabelC '>'; c=[$2]} }
	| LT me			{ {l=LabelC '<'; c=[$2]} }
;


%%

