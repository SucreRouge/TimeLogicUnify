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
let digit = ['0'-'9']
let atm   = lower (lower|digit)* 
rule token = parse
  | [' ' '\t' '\n']	{ token lexbuf }
  | "<-"         { XLT }
  | '<'         { LT }
  | '>'         { GT }
  | "->"         { XGT }
  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | ','		{ COMMA }
  | atm as str{ ATOM (str) }
  | '{'         { LBRACE }
  | '}'         { RBRACE }
  | '[' 	{ LSQUARE }
  | ']'		{ RSQUARE }
  | ';'		{ SEMICOLON }
  | '+'		{ PLUS }
  | _ as c 	{ Printf.printf "Ignoring unrecognized character: %c in me\n" c; token lexbuf}
  | eof 	{ EOF }
  (* | _		{ token lexbuf } *)
