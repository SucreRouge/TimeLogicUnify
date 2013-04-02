(* file: lexer.mll *)
(* Lexical analyzer returns one of the tokens:
   the token NUM of a floating point number,
   operators (PLUS, MINUS, MULTIPLY, DIVIDE, CARET, UMINUS),
   or NEWLINE.  It skips all blanks and tabs, and unknown characters
   and raises End_of_file on EOF. *)

{
open Phi_parser
open Char
include Char
}
let lower = ['a'-'z']
let atom  = (['a'-'z']|'_')+
let biary = ('&'|'|'|'<'|'>'|'Z')
let pre2 = ('U'|'S')
let digit = ['0'-'9']
let atm   = (lower (lower|digit)*|'0'|'1') 
rule token = parse
  | [' ' '\t']	{ token lexbuf }
  | biary as c  { BINARY(Char.escaped c) }
  | '^'		{ BINARY("&") }
  | '='		{ EQUALS("=") }
  | '-'		{ UNI("-") }
  | '~'		{ UNI("-") }
  | ['A' 'E' 'F' 'G' 'X'] as c { UNI(Char.escaped c) } 
  | 'N'         { UNI(Char.escaped 'X') } 
  | pre2 as c   { PREFIX(Char.escaped c) }
  | 'S'		{ SINCE }
  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | ','		{ COMMA }
  | atm    as c { ATOM (c) }
  | _ as c 	{ Printf.printf "Ignoring unrecognized character: %c\n" c ; token lexbuf}
  | eof		{ EOF }

  (* | _	'	{ token lexbuf } *)
