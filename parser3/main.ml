(* file: main.ml *)

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
*)

let printf=Printf.printf
let clear_equals = fun s -> let p = String.index s '=' in String.sub s (p+1) (String.length s - p - 1)
let rec fixstr_ s i j = if j >= String.length s then String.sub s 0 i else let (si,jj)=(match s.[j] with '%' -> (char_of_int (int_of_string ("0x"^(String.sub s (j+1) 2))) ,j+3) | '+' -> (' ',j+1) | _ -> (s.[j],j+1)) in s.[i]<-si ; fixstr_ s (i+1) jj 
let fixstr = fun s -> fixstr_ (clear_equals s) 0 0

let split_string s =
	let ind = String.index s ':' in 
	(String.sub s 0 ind, String.sub s (ind+1) ((String.length s)-ind-1))

let newline_split s ind =
	String.sub s 0 ind, String.sub s (ind) ((String.length s)-ind)

let append_char s c = s ^ (String.make 1 c)

let reverse s_ =
	let s = String.copy s_ in 
	let last_pos = (String.length s) - 1  in
	for i = 0 to (last_pos / 2)
	do
		let j = last_pos - i in
		let (c,d) = (s.[i],s.[j]) in
		s.[i] <- d ; s.[j] <- c
	done ; s
		
let string_maps f s = 
	let out = Buffer.create (2 * String.length s) in 
	String.iter (fun c -> (Buffer.add_string out (f c))) s;
	Buffer.contents out
let string_map f = string_maps (fun c -> String.make 1 (f c))

let split_every_char = string_maps (fun c -> (String.make 1 c ^ " "))
let v_to_or =  string_map (fun c -> if c='v' then '|' else c)

(*	let out = Buffer.create (2 * String.length s) in 
	let add = (Buffer.add_char out) in 
	(String.iter s) (fun c -> add c; add ' ') ;
	Buffer.contents out*)

let fix_braces str = 
	let openb_  = "{([" in
	let closeb_ = "})]" in
	let stack = ref [] in
	let need_rev = ref false in
	let fix openb closeb s = (
		let out = Buffer.create (2 * String.length str) in 
		let add = (Buffer.add_char out) in
		String.iter ( fun ch ->
			( try 
				let brace_type = String.index openb ch in
				stack := brace_type::(!stack) ;
			with Not_found -> try
				let brace_type = String.index closeb ch in
				while (!stack != [] && List.hd (!stack) != brace_type)
				do 
					let missing_close = closeb.[List.hd (!stack)] in 
					printf "Added missing `%c' prior to `%c'\n" missing_close ch;
					add missing_close ;
					stack := List.tl (!stack) 
				done ;
				if (!stack == [])
				then	need_rev := true
					(*let missing_open = openb.[brace_type] in 
					printf "Added missing `%c'  to beginning of string\n" missing_close ch;
					(*printf "Removed redundant `%c'\n" ch*)
					missing_open_braces := ' missing_open_braces + 
						*)
				else
					stack := List.tl (!stack) 
			with Not_found -> () 
			) ; add ch )  s;
		while (!stack != [])
		do 
			let missing_close = closeb.[List.hd (!stack)] in 
			printf "Added missing `%c'\n" missing_close;
			add missing_close ;
			stack := List.tl (!stack) 
		done ;
		Buffer.contents out) in
	let fixed1 = fix openb_ closeb_ str in
	let final_output = 
		if (!need_rev)
		then ( 
			let fixed2 = fix closeb_ openb_ (reverse fixed1) in
			(*printf "outr2: %s\n" fixed2;*)
			reverse fixed2
		) else 
			fixed1 
	in
	if (str = final_output) 
	then ()
	else printf "After adding missing brackets, \n\t\"%s\"\nbecomes\n\t\"%s\"\n" str final_output ;
	flush stdout;
	final_output;
;;
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


let robust_parse fixers_ par lex s_ =
	let rec parse fixers s =
		try (s, par lex (Lexing.from_string s))
		with Parsing.Parse_error -> match fixers with
			[] -> raise Parsing.Parse_error
			| fix::fixers_tl -> parse fixers_tl (fix s) in
	let (fixed_s, result) = parse fixers_ (fix_braces s_) in
	if (fixed_s <> s_) 
		then printf "Parsed OK, but needed to transform your string into \n\t%s\n" fixed_s
		else ();
	result

let log_dir= try 
	(Sys.getenv "HOME") ^ "/.config/"
with Not_found -> "/var/www/.config/"

let print_count = fun () -> Printf.printf "\nThis ME checker has been used %s times\n" 
(try 
        let stats_fname = log_dir ^ "mechecker_stats.txt" in
        let old_count = try
        	let input = open_in stats_fname in
		let cnt = input_line input in 
		close_in input;
		cnt;
	with _ -> "0" in
        let count = string_of_int (1+ int_of_string (old_count)) in
        let output = open_out stats_fname in
        output_string output count;
        close_out output;
        count
with _ -> "UNKNOWN")
        
let log f_name s =
        let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o600 f_name
        in
          output_string oc (s^"\n");
            close_out oc  

let do_model_check_string s =
        let status = ref "bad" in
	( try 
		let (formula_s, me_s) = split_string s in
		let formula_tree = robust_parse [split_every_char; v_to_or] Phi_parser.formula Phi_lexer.token formula_s in
		(*print_string (formula_s ^ "\n");*)
		(try 
			let me_tree = robust_parse [] Me_parser.me Me_lexer.token me_s in
			(*print_string (me_s ^ "\n"); *)
                        Me.do_model_check formula_tree me_tree;
                        status := "ok" 
		with Parsing.Parse_error-> print_string "Could not parse ME.\n")
        with 
	  Parsing.Parse_error-> print_string "Could not parse Formula.\n"
        | Not_found -> print_string "Is there a `:' in your input? \nIt is needed to seperate the formula from the me\n")
	;	
        print_count ();
        log (log_dir ^ "mechecker_" ^ !status ^ ".log") 
                (string_map (fun c->if c='\n' then ' ' else c) s)

;;

let main () =
  print_string "main loop";
  try
    while true do
      try 
	print_string "\n# ";
	let line = read_line() in
	print_string (line ^ "\n"); flush stdout;
	do_model_check_string (line)
      with
	  Parsing.Parse_error -> Printf.printf "Parse Error!\n" 
       	| Not_found -> print_string "Divider `:' not found in input."
    done;
(*    let _ = print_string ((Sys.getenv "QUERY_STRING")^"\n\n") ; flush stdout in *)
(*    let _ = print_string ((fixstr (Sys.getenv "QUERY_STRING")^"\n")) ; flush stdout in

    Calc.input Lexer.token (Lexing.from_string (fixstr (Sys.getenv "QUERY_STRING"))); flush stdout;
  *)     
  with End_of_file -> (print_string "EOF\n" ; flush stdout; exit 0)

let _ = 
try
(*	fix_braces "f)";
	exit 0;*)
     	Printf.printf "Content-type: text/plain\n\n";
	let qs = Sys.getenv "QUERY_STRING" in
	( try 
    		  do_model_check_string (fixstr qs); flush stdout;
	with Parsing.Parse_error -> Printf.printf "Parse Error!\n"
		| Not_found -> failwith "QUERY_STRING missing `='?\n" )
with 
	Not_found -> Printexc.print main () 
	|  _ -> printf "Unexpected error\n"

