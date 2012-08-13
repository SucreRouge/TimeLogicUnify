%{
open Me
include Me
%}

%token EOF
%token LPAREN RPAREN
%token COMMA OR
%token LBRACE RBRACE SEMICOLON LSQUARE RSQUARE GT LT IN PLUS XGT XLT
%token <string> ATOM
%token <char> UNARY

%left COMMA
%left SEMICOLON 
%left PLUS 
%left XLT LT GT XGT 

%start me
%type <Me.parse_t Me.tree> me

%%
me:	m EOF			{ $1 }
	| m2 EOF		{ $1 } 
/*	| m3 EOF		{ $1 }  */
;

/* Now to define "m", based on our canonical syntax */

shuflist:  /* empty */		{ [] }
	| m SEMICOLON shuflist	{ $1::$3 }
	| m COMMA shuflist	{ $1::$3 }
	| m shuflist		{ $1::$2  }
;
atomlist:  /* empty */		{[]}
	| ATOM COMMA atomlist	{ $1::$3 }
	| ATOM atomlist	{ $1::$2 }
;
m:	  ATOM			{ {l=ParseS [$1];c=[]} }
	| LBRACE atomlist RBRACE	{ {l=ParseS $2;c=[]} }
	| LSQUARE shuflist RSQUARE	{ {l=ParseC 'S';c=$2} }
	| LSQUARE RSQUARE	{ {l=ParseC 'S';c=[]} }
	| m PLUS m		{ {l=ParseC '+'; c=[$1;$3] } }
	| LT m			{ {l=ParseC '<'; c=[$2]} }
	| GT m			{ {l=ParseC '>'; c=[$2]} }
	| XLT m			{ {l=ParseC '<'; c=[$2]} }
	| XGT m			{ {l=ParseC '>'; c=[$2]} }
	| LPAREN m RPAREN	{ $2 }
;

/* Now we define m2, Like m but it uses <...> for shuffles and <-, -> for <,  > */

shuflist2: 
	| m2 SEMICOLON shuflist2	{ $1::$3 }
	| m2 COMMA shuflist2	{ $1::$3 }
	| m2 shuflist2		{ $1::$2  }
	| m2 			{ [$1] }
;

m2:	  ATOM			{ {l=ParseS [$1];c=[]} }
	| LBRACE atomlist RBRACE	{ {l=ParseS $2;c=[]} }
	| LT shuflist2 GT	{ {l=ParseC 'S';c=$2} }
	| m2 PLUS m2		{ {l=ParseC '+'; c=[$1;$3] } }
	| XLT m2		{ {l=ParseC '<'; c=[$2]} }
	| XGT m2		{ {l=ParseC '>'; c=[$2]} } 
	| LPAREN m2 RPAREN	{ $2 }
;

/* we also define m3 here, this is like m2, but use postfix notation, 
   we won't actually use m3, since it would confuse ocamlyacc to have
   too many alternative syntaxes 

shuflist3: 
	| m3 SEMICOLON shuflist3	{ $1::$3 }
	| m3 COMMA shuflist3	{ $1::$3 }
	| m3 shuflist3		{ $1::$2  }
	| m3 			{ [$1] }
;

m3:	  ATOM			{ {l=ParseS [$1];c=[]} }
	| LBRACE atomlist RBRACE	{ {l=ParseS $2;c=[]} }
	| LT shuflist2 GT	{ {l=ParseC 'S';c=$2} }
	| m3 PLUS m3		{ {l=ParseC '+'; c=[$1;$3] } }
	| m3 XLT		{ {l=ParseC '<'; c=[$1]} }
	| m3 XGT		{ {l=ParseC '>'; c=[$1]} } 
	| LPAREN m3 RPAREN	{ $2 }
; */
