(* file: main.ml 
 *  *)

open Mainlib
open Me (* only for type tree *)

(*type 'a tree = {l: 'a; c: 'a tree list
let tree = Me.tree*)



let rec format_tree t = let degree = List.length t.c in
        match degree with 
        0 -> t.l
        | 1 -> t.l ^ (format_tree (List.hd t.c))
        | 2 -> "(" ^ (format_tree (List.hd t.c)) ^ t.l ^
                     (format_tree (List.nth t.c 1)) ^ ")"
        | _ -> failwith "Unexpected Error: invalid formula tree"

let tree_leafs t =
        let set_add m lst = if List.mem m lst then lst else m::lst in
        let rec f leafs t = if t.c = [] 
                then set_add t.l leafs 
                else List.fold_left f leafs t.c   
        in 
        List.sort (fun s1 s2 -> String.length s1 - String.length s2) (f [] t)

(* Replace all leafs in the tree with single characters. Useful if sat checker
 * only supports single character names *)

let remap_leafs t =
        let leafs = tree_leafs t in
        if List.length leafs > 26 then failwith "Cannot map more than 26 variables to letters";
        let a = Array.make 26 "" in
        let idx s = (Char.code (String.get s 0) - Char.code 'a') in
        let rec findfrom m i = if a.(i) = m 
                then i 
                else findfrom m (if i < 25 then i+1 else 0) in
        List.iter (fun m -> a.(findfrom "" (idx m)) <- m) leafs;
        let charof m = Char.escaped (Char.chr (Char.code 'a' + findfrom m (idx m) )) in
        let rec f t = if t.c = [] 
                then {l = charof t.l; c=[]}
                else {l =        t.l; c=List.map f t.c} 
        in
        f t





                
                


let do_string s =
        let status = ref "bad" in
	try 
		let formula_tree = parse_ctls_formula s in
		print_string ((format_tree formula_tree) ^ "\n");
                List.iter print_string (tree_leafs formula_tree);
		print_string ((format_tree (remap_leafs formula_tree)) ^ "\n");
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

