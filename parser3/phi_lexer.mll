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
let biary = ('&'|'|'|'='|'<'|'>'|'Z')
rule token = parse
  | [' ' '\t']	{ token lexbuf }
  | biary as ch { BINARY(Char.escaped ch)}
  | '^'		{ BINARY("&")}
  | '-'		{ NEG }
  | '~'		{ NEG }
  | 'U'		{ UNTIL }
  | 'S'		{ SINCE }
  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | ','		{ COMMA }
  | lower+ as ch{ ATOM (ch) }
  | _ as c 	{ Printf.printf "Unrecognized character: %c\n" c; raise (Failure "")  }
  | eof		{ EOF }

  (* | _	'	{ token lexbuf } *)
