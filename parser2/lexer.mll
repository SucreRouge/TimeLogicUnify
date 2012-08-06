(* file: lexer.mll *)
(* Lexical analyzer returns one of the tokens:
   the token NUM of a floating point number,
   operators (PLUS, MINUS, MULTIPLY, DIVIDE, CARET, UMINUS),
   or NEWLINE.  It skips all blanks and tabs, and unknown characters
   and raises End_of_file on EOF. *)

{
  open Calc
}
let lower = ['a'-'z']
rule token = parse
  | [' ' '\t']	{ token lexbuf }
  | '\n'	{ NEWLINE }
  | '^'		{ AND }
  | '&'		{ AND }
  | '|'		{ OR }
  | '-'		{ NEG }
  | '~'		{ NEG }
  | 'U'		{ UNTIL }
  | 'S'		{ SINCE }
  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | ','		{ COMMA }
  | lower as ch	{ ATOM (ch) }
  | _		{ token lexbuf }
  | eof		{ raise End_of_file }
