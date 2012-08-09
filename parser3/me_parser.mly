%{
open Me
include Me
%}

%token NEWLINE EOF
%token LPAREN RPAREN
%token COMMA OR
%token LBRACE RBRACE SEMICOLON LSQUARE RSQUARE GT LT IN PLUS
%token <string> ATOM
%token <char> UNARY

%left PLUS 

%start me
%type <Me.parse_t Me.tree> me

%%
me:	m EOF			{ $1 }
shuflist:  /* empty */		{ [] }
	| m COMMA shuflist	{ $1::$3 }
	| m			{ [$1] }
;
atomlist:  /* empty */		{[]}
	| ATOM COMMA atomlist	{ $1::$3 }
	| ATOM			{ [$1] }
;
m:	LBRACE atomlist RBRACE	{ {l=ParseS $2;c=[]} }
	| LSQUARE shuflist RSQUARE	{ {l=ParseC 'S';c=$2} }
	| m PLUS m		{ {l=ParseC '+'; c=[$1;$3] } }
	| UNARY m			{ {l=ParseC $1; c=[$2]} }
;
