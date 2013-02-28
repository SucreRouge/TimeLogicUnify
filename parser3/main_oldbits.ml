(*
let url_decoding = Str.regexp "%[0-9][0-9]"

let decoding str =
  let str = Str.matched_string str in
  let s = String.create 4 in
  s.[0] <- '0';
  s.[1] <- 'x';
  s.[2] <- str.[1];
  s.[3] <- str.[2];
  String.make 1 (Char.chr (int_of_string s))
 
let decode str =
  Str.global_substitute url_decoding decoding str

let url_encoding = Str.regexp "[^[:alnum:]]" 

let encoding str =
	let str = (Chr.code (String.get str 0))

*)

(*
et brace_type = String.index openb ch in
				append_char (fix s next brace_type::stack) 
				else if (String.contains closeb ch) 
				then ( 
					match stack with
					  [] -> (* remove redundant close *)
						fix s (s_pos+1) stack
					   | old_brace::stack_tl ->
						let brace_type = String.index closeb ch in
						if (old_brace != brace_type) 
						then closeb.(brace_type)::(fix cl stack_tl)
						else fix cl_tl stack_tl
				) else ch::(fix cl_tl stack)
	in fix (
		if (pos >= String.length(s)) 
		then
			match stack with 
				[] -> []
				| brace_type::remainder -> closeb.(brace_type)::(fix next remainder)
*)	
(*			match !stack with
				[] -> printf ( "close brace not matched at end of \n%s\n" (String.sub s 0 (i+1)))
				| (old_i,old_brace_type)::remainder ->
					if (old_brace_type != brace_type ) 
					then printf ( "`%c' was closed with `%c' in \n%s\n" (String.sub s (old_i) (i-old_i+1) ^ "\n") 
					else ();
					stack := remainder
		with try *)
(*
let check_braces s = 
	let openb  = "{([" in
	let closeb = "})]" in
	let stack  = [] ref in
	for i = 0 to String.length -1 s
	do  
		let ch = s.(i) in
		try 
			let brace_type = String.index openb ch in
			stack := (i,brace_type)::stack
		with Not_found -> try 
			let brace_type = String.index closeb ch in
			match !stack with
				[] -> printf ( "close brace not matched at end of \n%s\n" (String.sub s 0 (i+1)))
				| (old_i,old_brace_type)::remainder ->
					if (old_brace_type != brace_type ) 
					then printf ( "`%c' was closed with `%c' in \n%s\n" (String.sub s (old_i) (i-old_i+1) ^ "\n") 
					else ();
					stack := remainder
		with try 
*)

(*	fix_braces "f)";
	exit 0;*)

(*    let _ = print_string ((Sys.getenv "QUERY_STRING")^"\n\n") ; flush stdout in *)
(*    let _ = print_string ((fixstr (Sys.getenv "QUERY_STRING")^"\n")) ; flush stdout in

    Calc.input Lexer.token (Lexing.from_string (fixstr (Sys.getenv "QUERY_STRING"))); flush stdout;
  *) 
