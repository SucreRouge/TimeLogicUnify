(* file: main.ml 
 *
 * The is the main file used to do model checking. It is essentially a wrapper
 * around the model-checker found in me.ml *)

open Mainlib

(*let split_string = Mainlib.split_string
let parse_rtl_formula = Mainlib.parse_rtl_formula
let robust_parse = Mainlib.robust_parse
*)
let do_model_check_string s =
        let status = ref "bad" in
	( try 
		let (formula_s, me_s) = split_string s in
		let formula_tree = parse_rtl_formula  formula_s in
		(*print_string (formula_s ^ "\n");*)
		(try 
			let me_tree = robust_parse [] Me_parser.me Me_lexer.token me_s in
			(*print_string (me_s ^ "\n"); *)
                        Me.do_model_check formula_tree me_tree;
                        status := "ok" 
		with Parsing.Parse_error-> print_string "Could not parse ME.\n")
        with 
	  Parsing.Parse_error-> print_string "Could not parse Formula.\n"
        | Not_found -> print_string "Is there a `:' in your input? \nIt is needed to seperate the formula from the me\n"
	| _ -> print_string "Unexpected model checking error")
	;	
        Mainlib.print_count "ME checker" "mechecker_stats.txt";
        Mainlib.log (Mainlib.log_dir ^ "mechecker_" ^ !status ^ ".log") 
                (Mainlib.string_map (fun c->if c='\n' then ' ' else c) s)

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
    
  with End_of_file -> (print_string "EOF\n" ; flush stdout; exit 0)

let _ = (
try  do_model_check_string (Sys.argv.(1))
with Parsing.Parse_error -> Printf.printf "Parse Error!\n"
| _ -> 
try
    	Printf.printf "Content-type: text/plain\n\n";
	let qs = Sys.getenv "QUERY_STRING" in
	Me.max_size := 10000;
	( try 
    		  do_model_check_string (Mainlib.fixstr qs); flush stdout;
	with Parsing.Parse_error -> Printf.printf "Parse Error!\n"
		| Not_found -> failwith "QUERY_STRING missing `='?\n" )
with 
	Not_found -> Printexc.print main () 
	|  _ -> Printf.printf "Unexpected error in main loop\n"
)
