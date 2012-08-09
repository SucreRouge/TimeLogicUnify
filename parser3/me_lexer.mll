(* file: lexer.mll *)
(* Lexical analyzer returns one of the tokens:
   the token NUM of a floating point number,
   operators (PLUS, MINUS, MULTIPLY, DIVIDE, CARET, UMINUS),
   or NEWLINE.  It skips all blanks and tabs, and unknown characters
   and raises End_of_file on EOF. *)

{
  open Me_parser
}
let lower = ['a'-'z']
rule token = parse
  | [' ' '\t']	{ token lexbuf }
  | '\n'	{ NEWLINE }
  | ('<'|'>') as ch { UNARY(ch) }
  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | ','		{ COMMA }
  | lower+ as str{ ATOM (str) }
  | '{'         { LBRACE }
  | '}'         { RBRACE }
  | '[' 	{ LSQUARE }
  | ']'		{ RSQUARE }
  | ';'		{ SEMICOLON }
  | '+'		{ PLUS }
  | _ as c 	{ Printf.printf "Unrecognized character: %c\n" c; raise (Failure "")  }
  | eof 	{ EOF }
  (* | _		{ token lexbuf } *)
