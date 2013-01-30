(* file: main.ml 
 *
 * The is the main file used to do model checking. It is essentially a wrapper
 * around the model-checker found in me.ml *)

open Mainlib

let do_string s =
        let status = ref "bad" in
	try 
		let formula_tree = parse_ctls_formula s in
                ()
		(*print_string (formula_s ^ "\n");*)
         with 
	  Parsing.Parse_error-> print_string "Could not parse Formula.\n"
	;	
        Mainlib.print_count "Program" "unify_stats.txt";
        Mainlib.log (Mainlib.log_dir ^ "unify_" ^ !status ^ ".log") 
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
	do_string (line)
      with
	  Parsing.Parse_error -> Printf.printf "Parse Error!\n" 
       	| Not_found -> print_string "Divider `:' not found in input."
    done;
    
  with End_of_file -> (print_string "EOF\n" ; flush stdout; exit 0)

let _ = 
try
    	Printf.printf "Content-type: text/plain\n\n";
	let qs = Sys.getenv "QUERY_STRING" in
	Me.max_size := 10000;
	( try 
    		  do_string (Mainlib.fixstr qs); flush stdout;
	with Parsing.Parse_error -> Printf.printf "Parse Error!\n"
		| Not_found -> failwith "QUERY_STRING missing `='?\n" )
with 
	Not_found -> Printexc.print main () 
	|  _ -> Printf.printf "Unexpected error\n"

