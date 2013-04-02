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

(*
let list_mapi f ll = let rec r n l = match l with [] -> [] | e::rem -> (f n e)::(r (n+1) rem) in r 0 ll;;                
let rec ww t = [] :: (List.concat (list_mapi (fun n e -> List.map (fun ee -> n::ee) (ww e)) t.c));;
 *)

let origcwd = Sys.getcwd ()

let _ = Unix.chdir "/var/data/unify"

let append_s_to_fname_ l s fname =
  let f = open_out_gen l  0o666 fname in
    output_string f s;
    close_out f

let append_s_to_fname = append_s_to_fname_ [Open_append] ;;
let appendc_s_to_fname = append_s_to_fname_ [Open_append; Open_creat] ;;

let split = Str.split (Str.regexp " +");;
let settings_solvers = ref (try split (Sys.getenv "UNIFY_SOLVERS") with _ -> ["mlsolver"; "BPATH"])
let settings_simplify = ref true

(* let get_argv_int n default =
    if (Array.length (Sys.argv)) > n 
    then int_of_string Sys.argv.(n) 
    else default  *)
 
let getenv_kd k d = try Sys.getenv k with Not_found -> d

(* split a string a position n and return (l)eft or (r)ight part *)
let split_at_n_l s n =  String.sub s 0 n  
let split_at_n_r s n = let len = String.length s in
  String.sub s n (len - n)

(*let max_runtime = try getenv "MAX_RUNTIME" with Not_found -> "2" 
let max_concurrent = try int_of_string (getenv "MAX_CONCURRENT") with Not_found -> 2  *)
let (max_runtime, max_concurrent) = try ignore(Sys.getenv "UNIFY_OFFLINE"); ("3600", 1) with Not_found -> (getenv_kd "UNIFY_TIMEOUT" "3", NUM_CPUS) ;;
let max_runtime_float = float_of_string max_runtime 
let verbose = false 

(* From: http://www2.lib.uchicago.edu/keith/ocaml-class/complete.html *)
let cat fname =
  Printf.printf "**** INTBegin CAT%s\n" fname;
  if (Sys.file_exists fname) then (
    let chan = open_in fname in
    let size = 4 * 1024 in
    let buffer = String.create size in
      Printf.printf "**** Begin CAT%s\n" fname;
      let eof = ref false in
        while not !eof do
          let len = input chan buffer 0 size in
            if len > 1
            then print_string (String.sub buffer 0 len)
            else eof := true
        done; ()
  ) else (
    print_string (fname ^ " does not exist\n")
  );
;;

(* From StackOverflow, I think *)
let read_all_lines file_name =
  let in_channel = open_in file_name in
  let rec read_recursive lines =
    try
      Scanf.fscanf in_channel "%[^\r\n]\n" (fun x -> read_recursive (x :: lines))
    with
      End_of_file ->
        lines in
  let lines = read_recursive [] in
  let _ = close_in_noerr in_channel in
  List.rev (lines);;

let is_sat_regexp = Str.regexp_case_fold "is satisfiable";;
let is_sat_regexp = Str.regexp " unsatisfiable"

let title_unsat_str =  "  UNsatisfiable: " 
let result_parse_info = [ ( Str.regexp_case_fold "is satisfiable", "  Satisfiable: ");
                          ( Str.regexp " unsatisfiable",           title_unsat_str) ]
let add_ref_list a : string list ref * 'a = (ref [],a)
let result_info = List.map add_ref_list result_parse_info
let clear_result_info () = List.iter (fun e -> (fst e) := []) result_info
(*let result_info = List.map (fun a -> (ref [],a)) result_parse_info
*)(*let result_info = [ ( Str.regexp_case_fold "is satisfiable", "Satisfiable: "  , ref[]);
                    ( Str.regexp " unsatisfiable",           "UNsatisfiable: ", ref[]) ] *)

(* From: http://www2.lib.uchicago.edu/keith/ocaml-class/complete.html *)

(*let cat filename =
 Printf.printf "*AT %s\n" filename
 type 'a tree = {l: 'a; c: 'a tree list
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


(*
 type 'a tree = {l: 'a; c: 'a tree list}
 let t0 = {l="p"; c=[]}
 let t1 = {l="q"; c=[]}
 let t2 = {l="&"; c=[t0;t1]}
 let t2b = {l="&"; c=[t1;t0]}
 let t3 = {l="&"; c=[t0;t0]};;

 *)
let isleaf t = t.c = []
module StringMap = Map.Make (String)

exception Not_matched
(*exception Not_matchedt *)
let stringmap_set x y m = 
  (try
    (if ((StringMap.find x m) <> y) then raise Not_matched)
  with Not_found -> ());
  StringMap.add x y m

let isvar_c c = match c with 'a' .. 'z' -> true | _ -> false;;
let isvar_s s = isvar_c (String.get s 0);;
let isvar t = isvar_s t.l;;

let rec format_tree t = let degree = List.length t.c in
  match degree with
      0 -> t.l
    | 1 -> t.l ^ (format_tree (List.hd t.c))
    | 2 -> "(" ^ (format_tree (List.hd t.c)) ^ t.l ^
           (format_tree (List.nth t.c 1)) ^ ")"
    | _ -> failwith "Unexpected Error: invalid formula tree";;


let match_tree_pattern t p =
  (* Printf.printf "T: %s %s\n" (format_tree t) (format_tree p); *)
  let map = ref StringMap.empty in
  let rec f t p = (
  (*Printf.printf "Test: %s %s\n" (format_tree t) (format_tree p);*)
        (*let degree = List.length p.c in;
        if degree > 0 then*)
        if isvar p then (
                map := (stringmap_set p.l t (!map)) 
        ) else (
          (if t.l <> p.l then raise Not_matched);
          List.iter2 f t.c p.c
        ) (* ensure variable maps *)
  ) in
    f t p; (!map)

(*let tree_x_map (isleaf : string -> bool) f t = *)
let tree_x_map (isleaf : string Me.tree -> bool) f t =
  (*ignore (isleaf : string Me.tree -> bool);*)
  let rec r t =
        if isleaf t 
        then (f t.l)
        else {l=t.l; c=(List.map r t.c)}
  in (r t);;

let tree_leaf_map = (tree_x_map isleaf)
let tree_var_map  = (tree_x_map isvar)

let apply_rule t (a,b) =
  let map = match_tree_pattern t a in
    tree_var_map (fun e -> StringMap.find e map) b

let parse_rule s =
      let t = parse_ctls_formula s in
        match t with 
            {l="="; c=[x;y]} -> (x,y)
          | _ -> failwith "Invalid =";;
(*
let my_rules = List.map parse_rule ["(--p)=p"; 
                         "(p&q)=(q&p)";
                         "(p|q)=(q|p)";
                         "(p&(q&r))=((p&q)&r)";
                         "((p&q)&r)=((r&q)&p)";
                         "((p&q)&r)=(p&(q&r))";
                         "(p<q)=(q>p)";
] ;;
 *)
let rule_fname = "out/rules.txt"
let rule_fname = "/var/www/urules.txt";;

let rule_descriptions = ref (read_all_lines rule_fname)
let rule_list = ref (List.map parse_rule (!rule_descriptions))
let list_append l e = List.rev (e::(List.rev l))

let rule_found = ref false

let add_rule t = (
   let s = (format_tree t) in
     Printf.printf "Testing potential rewrite rule %s ; %s\n" s t.l;
  if (t.l = "=") then (
   let s = (format_tree t) in
   if ((List.mem s (!rule_descriptions))) then (
     print_endline ("Is existing rule: " ^ s) 
    ) else ( if (List.hd t.c) = (List.hd (List.tl t.c)) 
    then  print_endline ("Ignoring trivial rewrite rule of form p=p: " ^ s) 
    else (
    rule_descriptions := list_append (!rule_descriptions) s;
    rule_list := list_append (!rule_list) (parse_rule s);
    (*appendc_s_to_fname (s^"\n") rule_fname*)
     append_s_to_fname_ [Open_append; Open_creat] (s^"\n") rule_fname;
     print_endline ("Added new rule: " ^ s)
   ))
));;
  (*if ((t.l = "=") and (not (List.mem s (!rule_descriptions)))) then *)


let process_file name fname t =
  if verbose then Printf.printf "**** Begin %s\n" name;
  if (Sys.file_exists fname) then (
    let chan = open_in fname in
    let size = 4 * 1024 in
    let buffer = String.create size in
    let len = input chan buffer 0 size in
    let s = (String.sub buffer 0 len) in
    let runtime_s = try
      let r = Str.regexp "RUNTIME: \\([0-9.]*\\)" in
        ignore(Str.search_forward r s 0);
        let time_s = Str.matched_group 1 s in
        Printf.sprintf "(%0.2f)" (float_of_string time_s)
    with Not_found -> "(?.?)" in
    if verbose then print_string (s);
    if (len >= size) then print_string "Data file too long\n";
    List.iter (fun x ->
                 let (l,(regexp,title)) = x in
                 let result_str = (name^runtime_s) in
                 try ignore(Str.search_forward regexp s 0) ;
                     (match (title, name, t) with
                         (* "  UNsatisfiable: "title_unsat_str, "BPATH", {l="-";
                          * c=[rule]}) -> *)
                         ( "  UNsatisfiable: ", ("BPATH" | "BPATHUE"), {l="-"; c=[rule]}) -> 
                                rule_found := true;
                           (*if (title == title_unsat_str) then (
                           Printf.printf "Title is %s\n" title ;*) add_rule rule
                           (* ) *)
                        | _ -> ());
                     Printf.printf "%s%s\n" title result_str; flush stdout;
                     l := (name^runtime_s)::(!l)
                 with Not_found -> ()
    ) result_info
  ) else (
    print_string (fname ^ " does not exist\n")
  );
  if verbose then Printf.printf "**** End %s\n" fname;
  ()

let rec format_tree_prefix t = t.l ^ (String.concat "" (List.map format_tree_prefix t.c))
let parse_tree_prefix s = let rec r i = (
  let opc = s.[i] in 
  let op = String.make 1 opc in 
    match opc with 
    '=' | '<' | '>' | 'U' | 'W' | '|' | '&' -> 
        let (i0,c0) = r (i+1) in
        let (i1,c1) = r (i0+1) in (i1, {l=op; c=[c0;c1]})
    | '-' | 'G' | 'F' | 'A' | 'E' | 'N' | 'X' -> 
        let (i0,c0) = r (i+1) in (i0, {l=op; c=[c0]})
    | '0' | '1' | 'a' .. 'z'  -> (i, {l=op;c=[]})) in
  snd (r 0)
;;
let clean_tree_prefix s = format_tree_prefix (parse_tree_prefix s) ;;

let tree_length t = String.length (format_tree_prefix t)

let simpler_than t1 t2 =
  let (ft1, ft2) = (format_tree_prefix t1, format_tree_prefix t2) in
  let (lt1, lt2) = (String.length ft1, String.length ft2) in
  if (lt1 < lt2) then true else (
    if (lt2 < lt1) then false else (
     if t2.l = "<" then true else ( 
       ft1 < ft2 
     )
    )
  )

(* Version of simpler than that just picks shortest formula

 let simpler_than t1 t2 =
  let (ft1, ft2) = (format_tree_prefix t1, format_tree_prefix t2) in
  let (lt1, lt2) = (String.length ft1, String.length ft2) in
  if (lt1 < lt2) then true else (
    false
  )

 *)

let simplify_root rules t_in =
  let t = ref t_in in
  let finished = ref false in 
  let simplify1 rule = (
    try ( 
      let t2 = apply_rule (!t) rule in
      if (simpler_than t2 (!t)) then ( Printf.printf "subst %s -> %s\n" (format_tree (!t)) (format_tree t2) ; t := t2; finished := false)
    ) with Not_matched -> ()) in
  while (not (!finished)) do
    finished := true;
    List.iter simplify1 rules;
  done;
  (!t)

(*let simplify_root rules t_in =
  let t = simplify_root_ rules t_in *) 

let rec simplify rules t = 
  if t.c = [] 
  then t
  else (simplify_root rules {l=t.l; c= List.map (simplify rules) t.c})



(*  Simplify will use e.g
 *  (Xa|Xa) -> X(a|a)
 *  but then not be able to use a|a -> a
 *  This reruns the loop over again *)
let rec simplify_star t_in =
  let rules = (!rule_list) in
  let t = ref t_in in
  let t_new = ref (simplify rules t_in) in
  while (not ((!t) = (!t_new))) do
    printf "  .%s\n  .%s\n" (format_tree (!t)) (format_tree (!t_new));
    t := !t_new;
    t_new := simplify rules (!t);
    printf "  %s\n  %s\n" (format_tree (!t)) (format_tree (!t_new))
               
  done;
  (!t)

let rec format_tree2 t = let degree = List.length t.c in
let l = match t.l with
    "<" -> "<=="
  | ">" -> "==>"
  | "=" -> "<==>"
  | "-" -> "~"
  | "0" -> "((a) & (~ a))"
  | "1" -> "((a) | (~ a))"
  | _ -> t.l in
  match degree with
      0 -> l
    | 1 -> l ^ " " ^ (format_tree2 (List.hd t.c))
    | 2 -> "((" ^ (format_tree2 (List.hd t.c)) ^ ") " ^ l ^ " (" ^
           (format_tree2 (List.nth t.c 1)) ^ "))"
    | _ -> failwith "Unexpected Error: invalid formula tree"

let tree_vars t =
  let set_add m lst = if List.mem m lst then lst else m::lst in
  let rec f leafs t = if isvar t
  then set_add t.l leafs
  else List.fold_left f leafs t.c
  in
    List.sort (fun s1 s2 -> String.length s1 - String.length s2) (f [] t)


(* Replace all leafs in the tree with single characters. Useful if sat checker
 * only supports single character names *)

let remap_leafs_ m t =
  let leafs = tree_vars t in
    if List.length leafs > 26 then failwith "Cannot map more than 26 variables to letters";
    let a = Array.make 26 "" in
    let idx s = m * (Char.code (String.get s 0) - Char.code 'a') in
 (*   let isvar s = (let i = idx s in i < 26 && i >= 0) in *)
    let rec findfrom m i = if a.(i) = m
    then i
    else findfrom m (if i < 25 then i+1 else 0) in
      List.iter (fun m -> if (isvar_s m)
                 then a.(findfrom "" (idx m)) <- m
                 else () ) leafs;
      let charof m = Char.escaped (Char.chr (Char.code 'a' + findfrom m (idx m) )) in
      let rec f t = if isvar t
      then {l = charof t.l; c=[]}
      else {l = t.l; c=List.map f t.c}
      in
        f t

let remap_leafs_preserve_first_char = remap_leafs_ 1
let remap_leafs = remap_leafs_ 0
let rename_variables = remap_leafs_ 0


let format_tree_mark f=(format_tree (remap_leafs f));;

(*let do_mlsolver t = ignore (Do_parallel.do_commands
 [|
 fun () ->
 Unix.execv "/home/john_large/src/mlsolver-1.1/mlsolver/bin/mlsolver"
 (*[| "../../4lsolver/lsolver/bin/mlsolver.exe"; "-pgs"; "recursive"; *)
 [| "/home/john_large/src/mlsolver-1.1/mlsolver/bin/mlsolver"; "-pgs"; "recursive";
 "-ve"; "-val"; "ctlstar"; format_tree2 t  |]
 |] 1.9 3);;

 let do_mark t  = (*let s = format_tree_mark t*)
 ignore (Do_parallel.do_commands
 [| fun () ->
 Unix.chdir "mark/";
 Unix.execvp "java"
 [| "java"; "-Djava.awt.headless=true"; "JApplet";
 format_tree_mark t; "CTL" |]
 |] 1.9 3);;
 *)

let redirect_output fname =
  let outfile = (Unix.openfile fname [Unix.O_CREAT; Unix.O_WRONLY] 0o644) in
  let _ = Unix.dup2 outfile Unix.stdout in
  let _ = Unix.dup2 outfile Unix.stderr in ()

let java_entry name = ( name, "mark/",  fun t fname ->
                          redirect_output "/dev/null";
                          Unix.chdir "mark/";
                          let args =
                            [| "java"; "-Djava.awt.headless=true"; "JApplet";
                               format_tree_mark t; name ; fname |] in
                            (*Unix.execvp "echo" args;*)
                            Unix.execvp "java" args )

(* Note mlsolver effectively adds an "A" before the formula, so we need to add 
 * an a so eg. -(EXp>Xp) is reported as satisfiable *)                          

let mlsolver_entry = ( "mlsolver", "", fun t fname ->
                         redirect_output fname;
                         Unix.execv "mlsolver/bin/mlsolver"
                           [| "mlsolver"; "-pgs"; "recursive";
                              "-ve"; "-sat"; "ctlstar"; "E " ^ (format_tree2 t)  |]
)

(* gives the canonical file name for a tree t: STUB *)
let md5 s = Digest.to_hex (Digest.string s)
let canonical_file t = md5 (format_tree_mark t)

let required_tasks t =
  let solver_entries = [mlsolver_entry; java_entry "BCTLNEW"; java_entry "BCTLOLD" ; java_entry "CTL"; java_entry "BPATH" ; java_entry "BPATHUE"; java_entry "BCTLHUE"  ] in
  let tasks = ref [] in
    List.iter  ( fun e -> 
                   let (solver_name, prefix, f) = e in
		   if List.mem solver_name (!settings_solvers) then (
                   let fname_ = "out/" ^ (canonical_file t) ^ "." ^ solver_name in
                   let fullfname_ = prefix ^ fname_ in
                   let fullfname3600 = fullfname_ ^ "3600" in
		   let fullfname = if (Sys.file_exists fullfname3600) then fullfname3600 else fullfname_ ^ max_runtime in
		   let fname = fname_ ^ max_runtime in
                   let task_f = (fun () -> f t fname) in
                   let on_finish_f1 = (fun () -> process_file solver_name fullfname t ) in
                     if (Sys.file_exists fullfname) then
                       on_finish_f1 ()
                     else
                       let on_finish_f runtime = (append_s_to_fname ("\nRUNTIME: "^(string_of_float runtime)^"\n") fullfname;
                       on_finish_f1 ()) in
                       tasks := (task_f, on_finish_f)::(!tasks)
    )) solver_entries ;
    Array.of_list(!tasks)

(*if not Sys.file_exists *)


let do_formula_tree formula_tree =
          let normal_s = (format_tree formula_tree) ^ "\n" in
      	  append_s_to_fname_ [Open_append;Open_creat] normal_s ("log." ^ max_runtime);
          (*List.iter print_string (tree_leafs formula_tree);
          print_string ((format_tree (remap_leafs formula_tree)) ^ "\n");
          print_string ((format_tree2 formula_tree) ^ "\n");
          do_mlsolver formula_tree ;
           do_mark formula_tree*)
          ignore (Do_parallel.do_commands (required_tasks formula_tree) max_runtime_float max_concurrent);

          if verbose then List.iter (fun x ->
                       let (l,(regexp,title)) = x in
                         print_string title;
                         List.iter (fun s -> print_string (s ^ " ")) (!l);
                         print_string "\n"
          ) result_info

let test_rule_ rule = do_formula_tree {l="-"; c=[rule]}
                        (*
let test_rule_ rule = Printf.printf "STUB: Test rule %s\n" (format_tree rule)
                        *)
let test_rule t1 t2 = if (tree_length t2) < (tree_length t1) then test_rule_ {l="="; c=[t1;t2]}  

let find_rule t =
  rule_found := false;
  (* let found = ref false in 
  let rule = ref {l=""; c=[]} in *)
  let rec r subtree = (
      if not (!rule_found) then  (
        List.iter r subtree.c;
        let rec rr simple = (
          if not (!rule_found) then (
            List.iter rr simple.c;
            test_rule subtree simple
          )
        ) in
        rr subtree
      )
  ) in
    r t;
   !rule_found

let rec simplify_learn t_in =
  let t = (simplify_star t_in) in
  if find_rule t then simplify_learn t else t





(*let do_mlsolver t = ignore (Do_parallel.do_commands (fun () ->  [|
 [| "../../4mlsolver/mlsolver/bin/mlsolver.exe"; "-pgs"; "recursive";
 "-ve"; "-val"; "ctlstar"; format_tree2 t  |]
 |] 1.9 3)  *)

let do_simplify my_simplify s =
  clear_result_info();
  let status = ref "bad" in
    try (
(*      print_endline origcwd; *)
      let formula_tree = parse_ctls_formula s in
        print_string ("Input formula: " ^ (format_tree formula_tree) ^ "\n");
        let f2 = rename_variables formula_tree in 
        print_string ("ORIG: " ^ (format_tree_prefix f2) ^ "\n");
        let f3 = (my_simplify f2) in
        print_string ("Simplified to: " ^ (format_tree f3) ^ "\n");
        print_string ("SIMP: " ^ (format_tree_prefix f3) ^ "\n");
        (*let f4 = (simplify (!rule_list) f3) in*)
        let orig_len = String.length (format_tree_prefix f2) in
        let simp_len = String.length (format_tree_prefix f3) in
        printf "LEN:	%d	%d\n" orig_len simp_len
    ) with
        Parsing.Parse_error-> print_string "Could not parse Formula.\n"
                                ;
                              Mainlib.print_count "Program" "unify_stats.txt";
                              Mainlib.log (Mainlib.log_dir ^ "unify_" ^ !status ^ ".log")
                                (Mainlib.string_map (fun c->if c='\n' then ' ' else c) s)
;;

let do_string s =
  clear_result_info();
  let status = ref "bad" in
    try (
(*      print_endline origcwd; *)
      let formula_tree = parse_ctls_formula s in
        print_string ("Input formula: " ^ (format_tree formula_tree) ^ "\n");
        let formula_tree = rename_variables formula_tree in 
        print_string ("Normalised to: " ^ (format_tree formula_tree) ^ "\n");
        print_string ("mlsolver fmt: " ^ (format_tree2 formula_tree) ^ "\n");
        let formula_tree = (if (!settings_simplify) 
		then simplify (!rule_list) (rename_variables formula_tree)
		else formula_tree) in
        (if (!settings_simplify) then print_string ("Simplified to: " ^ (format_tree formula_tree) ^ "\n"));
	do_formula_tree formula_tree;
        let formula_tree = {l="-"; c=[formula_tree]} in 
        print_string ("Negation: " ^ (format_tree formula_tree) ^ "\n");
	do_formula_tree formula_tree
    ) with
        Parsing.Parse_error-> print_string "Could not parse Formula.\n"
                                ;
                              Mainlib.print_count "Program" "unify_stats.txt";
                              Mainlib.log (Mainlib.log_dir ^ "unify_" ^ !status ^ ".log")
                                (Mainlib.string_map (fun c->if c='\n' then ' ' else c) s)
;;

(*let do_string__ s = do_string_ s ; do_string_ ("-(" ^ s ^ ")")*)

let main () =
  print_string "main loop";

  Sys.set_signal Sys.sigfpe
      (Sys.Signal_handle (fun _ -> print_string "blush\n" ; Printexc.print_backtrace stdout ; print_string "flush\n" ; flush stderr ; failwith "FPE")); 
  try 
    while true do
      try
        print_string "\n# ";
        let line = try read_line() with End_of_file -> (print_string "End of input\n" ; flush stdout; exit 0)  in
          print_string (line ^ "\n"); flush stdout;
          match line.[0] with 
              'R' -> cat rule_fname
            | '<' -> settings_simplify := false
            | '>' -> settings_simplify := true 
            | 'S' -> do_simplify simplify_star  (split_at_n_r line 1)
            | 'L' -> do_simplify simplify_learn (split_at_n_r line 1)
            | _ -> do_string (line)
      with
          Parsing.Parse_error -> Printf.printf "Parse Error!\n"
        | Not_found -> print_string "Divider `:' not found in input."
    done;
  with 
       | End_of_file -> (print_string "End of input??\n" ; flush stdout; exit 0) 
       | x -> print_string (Printexc.get_backtrace ()) ; print_string (Printexc.to_string x); failwith "Unexpected exception 1"

let _ =
  try
    Printf.printf "Content-type: text/plain\n\n";
    let qs = Sys.getenv "QUERY_STRING" in
      Me.max_size := 10000;
      ( try
	  settings_simplify := (try (Mainlib.get_url_argument "simplify" qs) = "y" with Not_found -> false);
          settings_solvers := (Mainlib.get_url_list "solver" qs);
          (*settings_exclude := " " ^ (Mainlib.get_url_argument "exclude" qs) ^ " ";*)
	  (*
          (* Printf.printf "XX%sXX\n" (Mainlib.get_url_argument "simplify" qs); flush stdout; *)
*)          
do_string (Mainlib.get_url_argument "i" qs); flush stdout;
        with Parsing.Parse_error -> Printf.printf "Parse Error!\n"
          | Not_found -> failwith "QUERY_STRING missing `='?\n" )
  with
      Not_found -> Printexc.print main ()
    |  x ->  print_string (Printexc.to_string x);Printf.printf "Unexpected error in unify main loop\n"


