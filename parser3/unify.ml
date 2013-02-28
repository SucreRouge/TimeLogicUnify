(* file: main.ml 
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
let url_encoding = Str.regexp "." 

#load "str.cma";;
let url_encoding = Str.regexp "[[^0-9A-Za-z_]]." 
let url_encoding = Str.regexp "[[^0-9]]" 

let encoding str = 
  let str = Str.matched_string str in
  ("%" ^ string_of_int (Char.code (String.get str 0)))
	
let encode str = Str.global_substitute url_encoding encoding str;;

let _ = encode "(p&q)";;


--- MD5 --- 
let d= Digest.string "f";;

let s = Digest.to_hex d;;

let _ = print_string s;;



 *  *)

open Mainlib
open Me (* only for type tree *)

(*type 'a tree = {l: 'a; c: 'a tree list
let tree = Me.tree*)

(* URL encoding *)
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


let url_encoding = Str.regexp "[^0-9a-zA-Z]" 

let encoding str =
  let str = Str.matched_string str in
  ("%" ^ Printf.sprintf  "%x" (Char.code (String.get str 0)))
        
let encode str = Str.global_substitute url_encoding encoding str;;



let rec format_tree t = let degree = List.length t.c in
        match degree with 
        0 -> t.l
        | 1 -> t.l ^ (format_tree (List.hd t.c))
        | 2 -> "(" ^ (format_tree (List.hd t.c)) ^ t.l ^
                     (format_tree (List.nth t.c 1)) ^ ")"
        | _ -> failwith "Unexpected Error: invalid formula tree"

let rec format_tree2 t = let degree = List.length t.c in
        let l = match t.l with
        "<" -> "<=="
        | ">" -> "==>"
        | "=" -> "<==>"
        | "-" -> "~" 
        | _ -> t.l in
        match degree with 
        0 -> l
        | 1 -> l ^ " " ^ (format_tree2 (List.hd t.c))
        | 2 -> "((" ^ (format_tree2 (List.hd t.c)) ^ ") " ^ l ^ " (" ^
                     (format_tree2 (List.nth t.c 1)) ^ "))"
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
        let isvar s = (let i = idx s in i < 26 && i >= 0) in
        let rec findfrom m i = if a.(i) = m 
                then i 
                else findfrom m (if i < 25 then i+1 else 0) in
        List.iter (fun m -> if (isvar m)
                then a.(findfrom "" (idx m)) <- m
                else () ) leafs;
        let charof m = Char.escaped (Char.chr (Char.code 'a' + findfrom m (idx m) )) in
        let rec f t = if t.c = [] 
                then {l = if isvar t.l then charof t.l else t.l; c=[]}
                else {l = t.l; c=List.map f t.c} 
        in
        f t

let format_tree_mark f=(format_tree (remap_leafs f))

let do_mlsolver t = ignore (Do_parallel.do_commands 
        [| 
                fun () ->
                Unix.execv "/home/john_large/src/mlsolver-1.1/mlsolver/bin/mlsolver"
                (*[| "../../4lsolver/lsolver/bin/mlsolver.exe"; "-pgs"; "recursive"; *)
                [| "/home/john_large/src/mlsolver-1.1/mlsolver/bin/mlsolver"; "-pgs"; "recursive";
                "-ve"; "-val"; "ctlstar"; format_tree2 t  |]
        |] 1.9 3)

let do_mark t  = let s = format_tree_mark t
ignore (Do_parallel.do_commands 
        [| fun () ->
                Unix.chdir "mark/";
                Unix.execvp "java"
                [| "java"; "-Djava.awt.headless=true"; "JApplet";
                   format_tree_mark t; "CTL" |]
        |] 1.9 3)

let mark_entry = [| "mark",  fname t = fun t fname ->
                Unix.chdir "mark/";
                Unix.execvp "java"
                [| "java"; "-Djava.awt.headless=true"; "JApplet";
                   format_tree_mark t; "CTL"; fname |] |]

(*let do_mlsolver t = ignore (Do_parallel.do_commands (fun () ->  [| 
        [| "../../4mlsolver/mlsolver/bin/mlsolver.exe"; "-pgs"; "recursive";
        "-ve"; "-val"; "ctlstar"; format_tree2 t  |]
        |] 1.9 3)  *)

let do_string s =
        let status = ref "bad" in
	try 
		let formula_tree = parse_ctls_formula s in
		print_string ((format_tree formula_tree) ^ "\n");
                List.iter print_string (tree_leafs formula_tree);
		print_string ((format_tree (remap_leafs formula_tree)) ^ "\n");
		print_string ((format_tree2 formula_tree) ^ "\n");
                do_mlsolver formula_tree ;
                do_mark formula_tree
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

