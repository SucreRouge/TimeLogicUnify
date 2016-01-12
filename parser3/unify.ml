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

module StringSet = Set.Make(String) ;; (* sets of strings *)

(* contains and replance based on stackoverflow *)
let contains s2 s1 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false

let replace input output =
    Str.global_replace (Str.regexp_string input) output


let _cache_result_sat = ref [""]
let _cache_result_unsat = ref [""]

(*
let list_mapi f ll = let rec r n l = match l with [] -> [] | e::rem -> (f n e)::(r (n+1) rem) in r 0 ll;;                
let rec ww t = [] :: (List.concat (list_mapi (fun n e -> List.map (fun ee -> n::ee) (ww e)) t.c));;
 *)

(*let origcwd = Sys.getcwd ()
(*let _ = Sys.command "/usr/bin/whoami" *)

(* Assume that if HOME is not set it is because we are being run in public-html/cgi-bin *)
let home = try Sys.getenv "HOME" with _ -> origcwd^"/../.."
*)
(*let home = try Sys.getenv "HOME" with _ -> print_string ("Could not find HOME, trying"^origcwd^"/../..") ; origcwd^"/../.."*)
(*let _ = Unix.chdir "/var/data/unify";;*)
let rule_fname  = try Unix.chdir "/var/data/unify" ; "/var/www/urules.txt" 
         with Unix.Unix_error (Unix.ENOENT, "chdir", _ )  ->  (*let publichtml = home ^ "/public-html/" in*)
		try Unix.chdir (Mainlib.publichtml^".data/unify") ; Mainlib.publichtml^"urules.txt" 
         		with Unix.Unix_error (Unix.ENOENT, "chdir", _ )  -> Unix.chdir ("work") ; "urules.txt" 

let rule_fname_TRS=rule_fname^".trs"
	

         (*with Unix.Unix_error (_, "chdir", _ )  ->  Unix.chdir "~/data/unify" ; "~/public-html/urules.txt"
let rule_fname = "/var/www/urules.txt";;
*)  

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
let getenvi_kd k d = try int_of_string (Sys.getenv k) with Not_found -> d

let settings_do_negation = (getenv_kd "UNIFY_DO_NEG" "Y") = "Y"
(* split a string a position n and return (l)eft or (r)ight part *)
let split_at_n_l s n =  String.sub s 0 n  
let split_at_n_r s n = let len = String.length s in
  String.sub s n (len - n)

(*let max_runtime = try getenv "MAX_RUNTIME" with Not_found -> "2" 
let max_concurrent = try int_of_string (getenv "MAX_CONCURRENT") with Not_found -> 2  *)
let (max_runtime, max_concurrent) = try 
  ignore(Sys.getenv "UNIFY_OFFLINE"); ("3600", 1) 
with Not_found -> (getenv_kd "UNIFY_TIMEOUT" "3", getenvi_kd "UNIFY_CPUS" NUM_CPUS) ;;
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
        done; close_in_noerr chan
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
let result_parse_info = [ ( Str.regexp_case_fold "is satisfiable", true);
                          ( Str.regexp " unsatisfiable",           false) ]
let add_ref_list a : string list ref * 'a = (ref [],a)
let result_info = List.map add_ref_list result_parse_info
let clear_result_info () = List.iter (fun e -> (fst e) := []) result_info
(*let result_info = List.map (fun a -> (ref [],a)) result_parse_info
*)(*let result_info = [ ( Str.regexp_case_fold "is satisfiable", "Satisfiable: "  , ref[]);
                    ( Str.regexp " unsatisfiable",           "UNsatisfiable: ", ref[]) ] *)

(* From: http://www2.lib.uchicago.edu/keith/ocaml-class/complete.html *)

(*let cat filename =
 Printf.printf "*AT %s\n" filename
 type 'a tree = {l: 'a; c: 'a tree list}
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

let known_lhs = Hashtbl.create 8;;

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
    (*| 1 -> t.l ^ "("^(format_tree (List.hd t.c))^")"
    | 2 -> "(" ^ (format_tree (List.hd t.c)) ^ t.l ^ *)
    | 1 -> t.l ^ (format_tree (List.hd t.c))
    | 2 -> "((" ^ (format_tree (List.hd t.c)) ^ ")"^t.l^"(" ^
           (format_tree (List.nth t.c 1)) ^ "))"
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

let tree_deequals t =
  (*ignore (isleaf : string Me.tree -> bool);*)
  let rec r t =
        let mapc = (List.map r t.c) in 
        if t.l = "=" 
        then ({l="&";c=[{l=">";c=mapc};{l="<";c=mapc}]} )
        else {l=t.l; c=mapc}
  in (r t);;


let tree_x_map_sign (isleaf : string Me.tree -> bool) f t_in =
  (*ignore (isleaf : string Me.tree -> bool);*)
  let op_sign_list = [('<',[1;(-1)]);
                ('>',[-1;1]);
                ('-',[-1]);
                ('&',[1;1]);
                ('|',[1;1]);
                ('U',[1;1]);
                ('=',[0;0]);
                ('F',[1]);
                ('G',[1]);
                ('A',[1]);
                ('X',[1]);
                ('0',[]);
                ('1',[]);
                ('E',[1])] in
  let rec r t_and_sign =
        let (t, sign) = t_and_sign in
        if isleaf t 
        then (f t sign)
        else ( try 
                 (
                 let op_sign = List.assoc (String.get t.l 0) op_sign_list in
                 let child_sign = List.map (fun i -> i * sign) op_sign in
                 {l=t.l; c=(List.map r (List.combine (t.c) (child_sign)))}
                 ) with Not_found -> (failwith ("\nUnknown Operator: " ^ t.l ^"\n"))
        )
  in  
    (r (t_in, 1))
 ;;

let tree_var_map_sign  = (tree_x_map_sign isvar)

let force_state_var t = (tree_var_map_sign (fun t i -> match i with
                          1 -> {l="A"; c = [t]}
                     | (-1) -> {l="E"; c = [t]}
                )) (tree_deequals t)

let force_state_var_A t = tree_var_map_sign (fun t i -> {l="A"; c = [t]}) t

let _ = format_tree (force_state_var {l="p"; c = []})

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
let rule_fname = "out/rules.txt"
 *)

let rule_descriptions = ref (read_all_lines rule_fname)
let rule_list = ref (List.map parse_rule (!rule_descriptions))
let list_append l e = List.rev (e::(List.rev l))

let rule_found = ref false
let store_rules = ref true

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
      rule_found := true;
      if (!store_rules) then (
        rule_descriptions := list_append (!rule_descriptions) s;
        rule_list := list_append (!rule_list) (parse_rule s);
        (*appendc_s_to_fname (s^"\n") rule_fname*)
        append_s_to_fname_ [Open_append; Open_creat] (s^"\n") rule_fname;
        print_endline ("Added new rule: " ^ s)
      ) else (
        print_endline ("Suppressed new rule: " ^ s)
      )
   ))
));;
  (*if ((t.l = "=") and (not (List.mem s (!rule_descriptions)))) then *)

let regexp_get_num r fmt default s = try
        ignore(Str.search_forward r s 0);
        let ret = Str.matched_group 1 s in
        Printf.sprintf fmt (float_of_string ret)
    with Not_found -> default

let runtime_regexp_ = Str.regexp "RUNTIME: \\([0-9.]*\\)"
let colour_regexp_ = Str.regexp " \\([0-9.]*\\) colours,"
let hue_regexp_ = Str.regexp " \\([0-9.]*\\) hues,"

let process_file_stats name fname t =
  let sat_s = ref "?" in
  if verbose then Printf.printf "**** Begin %s\n" name;
  if (Sys.file_exists fname) then (
    let chan = open_in fname in
    let size = 440 * 1024 in
    let buffer = String.create size in
    let len = input chan buffer 0 size in
    close_in_noerr chan;
    let s = (String.sub buffer 0 len) in
    let runtime_s = regexp_get_num runtime_regexp_ "%0.2f" "?.??" s in
    let  colour_s = regexp_get_num  colour_regexp_ "%0.0f"  "?"     s in
    let     hue_s = regexp_get_num     hue_regexp_ "%0.0f"  "?"     s in
    if verbose then print_string (s);
    if (len >= size) then print_string "Data file too long\n";
    List.iter (fun x ->
                 let (l,(regexp,issat)) = x in
                 let result_str = (name^"("^runtime_s^")") in
                 try ignore(Str.search_forward regexp s 0);
                     let title = if issat 
                        then (sat_s := "Y" ; _cache_result_sat   := name::!_cache_result_sat  ; "  Satisfiable: ") 
                        else (sat_s := "N" ; _cache_result_unsat := name::!_cache_result_unsat; "  UNsatisfiable: ") in
                     (match (issat, name, t) with
                         (* "  UNsatisfiable: "title_unsat_str, "BPATH", {l="-";
                          * c=[rule]}) -> *)
                         ( false, ("BPATH" | "BPATHUE"), {l="-"; c=[rule]}) -> if (rule.l="=") then add_rule rule;
                           (*if (title == title_unsat_str) then (
                           Printf.printf "Title is %s\n" title ; if !store_rules then add_rule rule *)
                           (* ) *)
                        | _ -> ());
                     Printf.printf "%s%s\n" title result_str; flush stdout;
                     l := (name^"("^runtime_s^")")::(!l)
                 with Not_found -> ()
    ) result_info;
    if verbose then Printf.printf "**** End %s\n" fname;
    [!sat_s; runtime_s; colour_s; hue_s]
  ) else (
    print_string (fname ^ " does not exist\n");
    []
  )

let process_file name fname t = ignore ( process_file_stats name fname t )


let process_file name fname t = Printf.printf "%s\n" (String.concat "\t" ( process_file_stats name fname t ))

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


 let formula_length t = String.length (format_tree_prefix t)
 (*let rule_length (x,y) = (formula_length x) + (formula_length y)*)
 
 let compare_rule r1 r2 = 
 	let rule2formula (x,y) = {l="="; c=[x;y]} in
	let (t1,t2) = (rule2formula r1, rule2formula r2) in
  	let (ft1, ft2) = (format_tree_prefix t1, format_tree_prefix t2) in
  	let (lt1, lt2) = (String.length ft1, String.length ft2) in
	if (lt1 = lt2) then 
  		let (ft1, ft2) = (format_tree t1, format_tree t2) in
  		let (lt1, lt2) = (String.length ft1, String.length ft2) in
  		compare lt1 lt2 
	else compare lt1 lt2

 (* Version of simpler than that just picks shortest formula
	Could simplify in terms of above function *)
 let shorter_than t1 t2 =
  let (ft1, ft2) = (format_tree_prefix t1, format_tree_prefix t2) in
  let (lt1, lt2) = (String.length ft1, String.length ft2) in
  if (lt1 < lt2) then true else (
    false
  )
  
  

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
  let used_rules = ref [] in
  printf "Num rules %d\n" (List.length rules);

  let t = ref t_in in
  let t_new = ref (simplify rules t_in) in
  List.iter ( fun e -> 
	used_rules:=((!used_rules)@[e]);
	let rules =(!used_rules) in
	t_new := simplify rules (!t);
  	while (not ((!t) = (!t_new))) do
		printf "  .%s\n  .%s\n" (format_tree (!t)) (format_tree (!t_new));
		t := !t_new;
		t_new := simplify rules (!t);
		printf "  %s\n  %s\n" (format_tree (!t)) (format_tree (!t_new))
  	done; ()
  ) rules;
  (!t)

(*let rec simplify_star_ t_in =
  let rules = (!rule_list) in
  printf "Num rules %d\n" (List.length rules);

  let t = ref t_in in
  let t_new = ref (simplify rules t_in) in
  while (not ((!t) = (!t_new))) do
    printf "  .%s\n  .%s\n" (format_tree (!t)) (format_tree (!t_new));
    t := !t_new;
    t_new := simplify rules (!t);
    printf "  %s\n  %s\n" (format_tree (!t)) (format_tree (!t_new))
               
  done;
  (!t) *)



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

(* From stackoverflow *)
let contains s2 s1 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false

let replace input output =
    Str.global_replace (Str.regexp_string input) output


(*let format_tree_TRS t =
	format_tree_TRS_(List.hd t.c)^"="^format_tree_TRS_(List.nth t.c 1)*)

let rec format_tree_tex t = let degree = List.length t.c in
let l = match t.l with
    "<" -> "\\leftarrow"
  | ">" -> "\\rightarrow"
  | "=" -> "\\leftrightarrow"
  | "-" -> "\\neg"
  | "~" -> "\\neg"
  | "0" -> "\\bot"
  | "1" -> "\\top"
  | "&" -> "\\wedge"
  | "|" -> "\\vee"
  | _ -> t.l in
  match degree with
      0 -> l
    | 1 -> l ^ " " ^ (format_tree_tex (List.hd t.c))
    | 2 -> "(" ^ (format_tree_tex (List.hd t.c)) ^ " " ^ l ^ " " ^
           (format_tree_tex (List.nth t.c 1)) ^ ")"
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

let remap_leafs__ first_char m t =
  let leafs = tree_vars t in
    if List.length leafs > 26 then failwith "Cannot map more than 26 variables to letters";
    let a = Array.make 26 "" in
    let idx s = m * (Char.code (String.get s 0) - Char.code first_char) in
 (*   let isvar s = (let i = idx s in i < 26 && i >= 0) in *)
    let rec findfrom m i = if a.(i) = m
    then i
    else findfrom m (if i < 25 then i+1 else 0) in
      List.iter (fun m -> if (isvar_s m)
                 then a.(findfrom "" (idx m)) <- m
                 else () ) leafs;
      let charof m = Char.escaped (Char.chr (Char.code first_char + findfrom m (idx m) )) in
      let rec f t = if isvar t
      then {l = charof t.l; c=[]}
      else {l = t.l; c=List.map f t.c}
      in
        f t

let remap_leafs_ = remap_leafs__ 'a'
let remap_leafs_preserve_first_char = remap_leafs_ 1
let remap_leafs = remap_leafs_ 0
let rename_variables = remap_leafs_ 0

let format_tree_mark f=(format_tree (remap_leafs f));;

(* write out tree in form readable by tradtional Term Rewrite System software *)
let rec format_tree_TRS_ t = let degree = List.length t.c in
let l = match t.l with
    "<" -> "impby"
  | ">" -> "implies"
  | "=" -> "eq"
  | "-" -> "-"
  | "0" -> "0"
  | "1" -> "1"
  | "&" -> "*"
  | "|" -> "+"
  | _ -> t.l in
  match degree with
      0 -> l
    | 1 -> l ^ "(" ^ (format_tree_TRS_ (List.hd t.c)) ^ ")"
    | 2 -> l ^ "(" ^ (format_tree_TRS_ (List.hd t.c)) ^ "," ^
           (format_tree_TRS_ (List.nth t.c 1)) ^ ")"
    | _ -> failwith "Unexpected Error: invalid formula tree"

let rec parse_tree_TRS_ ptr s =
	(*Printf.printf "%s %d\n" s ptr;*)
	let junk = ptr + 1 in
	let p = ref ptr in
	let sym () = String.make 1 s.[!p] in
	let chomp () = (while s.[!p] == ' ' do p:=1+(!p) done) in
	chomp ();
	(* should not be ( , or ) *)
	let label = match sym() with 
		  "]" -> ">"
		| "[" -> "<"
		| "*" -> "&"
		| "+" -> "|" 
		| _   -> sym() in
	p:=1+(!p); chomp ();
	let children = ref [] in
	(if s.[!p] == '(' then (
		(p:=1+(!p); chomp (););
		let read_child() = (
			let (np,nt) = parse_tree_TRS_ !p s in
			let junk = format_tree nt in
			p:=np;
			children := nt::(!children);
			chomp();
		) in
		if s.[!p] != ')' then read_child() else();
		while s.[!p] == ',' do
			(p:=1+(!p); chomp (););
			read_child();
		done;
		(if s.[!p] == ')' then
			p:=1+(!p)
		else failwith ("')' expected but have: " ^ (String.make 1 (s.[!p])) ) )
	) else (););
	let t = {l=label; c=(List.rev (!children))} in
	(*Printf.printf "%s, degree %d" (format_tree_mark t) *)
	(*print_endline (format_tree t);*)
(!p,t) 

let rec parse_tree_TRS_rule_ s =
	let s = replace "implies" ">" (s^") ") in
	let (p1,t1) = parse_tree_TRS_ 0 s in
	(*print_endline ("T1: "^(format_tree t1));*)
	(* Now skip over " -> ", of length 4 *)
	let (p2,t2) = parse_tree_TRS_ (p1+4) s in
	(*print_endline ("T2: "^(format_tree t2));*)
	(t1,t2)
	
let rule_descriptions_TRS = ref (List.filter (contains "->") (read_all_lines rule_fname_TRS))
let rule_list_TRS = ref (List.map parse_tree_TRS_rule_ (!rule_descriptions_TRS))
                        
let print_rules_tex =
	List.iter ( fun (x,y) -> Printf.printf "%s \\rewrite %s\n" 
		(format_tree_tex x)
		(format_tree_tex y)
	)
	

let format_tree_TRS t = format_tree_TRS_ t
	(*format_tree_TRS_(remap_leafs__ 'x' 0 t)*)

let dump_TRS() = 
  let rules = (!rule_list) in
  let rec r l = match l with 
	  (x,y)::l2 -> (format_tree_TRS x) ^ "=" ^ (format_tree_TRS y) ^ "\n" ^ (r l2)
	| _ -> "" in
  r rules

(* New TRS simplification functions *)

let simplify_root_TRS rules t_in =
  let t = ref t_in in
  let finished = ref false in 
  let simplify1 rule = (
    try ( 
      let t2 = apply_rule (!t) rule in
      (* if (simpler_than t2 (!t)) then ( Printf.printf "subst %s -> %s\n" (format_tree (!t)) (format_tree t2) ; t := t2; finished := false) *)
      ( Printf.printf "subst %s -> %s\n" (format_tree (!t)) (format_tree t2) ; t := t2; finished := false)
    ) with Not_matched -> ()) in
  while (not (!finished)) do
    finished := true;
    List.iter simplify1 rules;
  done;
  (!t)

let rec simplify_TRS rules t = 
  if t.c = [] 
  then t
  else (simplify_root_TRS rules {l=t.l; c= List.map (simplify_TRS rules) t.c})


(* Here I redirect the simplify_TRS to use CIME rather than internal TRS code
   This is because cime supports Associative-Commutative rules *)

let cimeplify t =
	let str_orig = format_tree t in
	let process = (Unix.open_process_in ("cime/simplify.sh '"^str_orig^"'")) in
	let str_cime = input_line process in
	Unix.close_process_in process;
	let t2 = parse_ctls_formula (str_cime) in
	print_string ("Cime: "^str_cime^"!\n");
	t2
;;

let simplify_star_TRS = cimeplify

(*  Simplify will use e.g
 *  (Xa|Xa) -> X(a|a)
 *  but then not be able to use a|a -> a
 *  This reruns the loop over again *)
let rec simplify_star_TRS_nocime t_in =
  let rules = (!rule_list_TRS) in
  printf "Num TRS rules %d\n" (List.length rules);

  let t = ref t_in in
  let t_new = ref (simplify_TRS rules t_in) in
  while (not ((!t) = (!t_new))) do
	printf "  .%s\n  .%s\n" (format_tree (!t)) (format_tree (!t_new));
	t := !t_new;
	t_new := simplify_TRS rules (!t);
	printf "  %s\n  %s\n" (format_tree (!t)) (format_tree (!t_new))
  done;
  (!t)

let simplify_star_both t =
	simplify_star (simplify_star_TRS t)

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

(* TODO: implement the unified BCTL+BCTL* unified solver from v2 rather than just use v1.0 *)
(* this is creates an entry for a java solver *)                                             
let java_entry name = ( name, "",  fun t fname ->
                          (*Unix.chdir "mark/";*)
                          print_string (name^"->"^fname^"\n");
                          let args =
                            [| "java"; "-classpath"; "mark/src/v1.0/src"; "-Djava.awt.headless=true"; "JApplet";
                               format_tree_mark t; name ; fname |] in
                            (*Unix.execvp "echo" args;*)
			    Array.iter print_string args;
			    print_string "\n";
                            redirect_output "/dev/null";
                            Unix.execvp "java" args )

(* As above but translates the formula so that all variables are forced to be
 * treated as state variables, even if we are using a non-local logic *)
let java_entry_f name = ( name ^ "f", "",  fun t fname ->
                          redirect_output "/dev/null";
                          (*Unix.chdir "mark/";*)
                          let args =
                            [| "java";"-classpath"; "mark/src"; "-Djava.awt.headless=true"; "JApplet";
                               format_tree_mark (force_state_var t); name ; fname |] in
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
  let solver_entries = [mlsolver_entry; java_entry "BCTLNEW"; java_entry "BCTLOLD" ; java_entry "CTL"; java_entry "BPATH" ; java_entry "BPATHUE";java_entry_f "BPATH" ; java_entry_f "BPATHUE";  java_entry "BCTLHUE"  ] in
  let tasks = ref [] in
    List.iter  ( fun e -> 
                   let (solver_name, prefix, f) = e in
		   if (List.mem solver_name (!settings_solvers)) || ((!settings_solvers=["*"])) then (
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

let equivalent_solvers = [["CTL"; "mlsolver"]; ["BCTL";"BCTLNEW"; "BCTLOLD"; "BCTLHUE"; "BPATHf"]; ["BPATH"; "BPATHUE"]]

let sat_implies = Hashtbl.create 40;;
let stronger = ref [] in List.iter (
  fun x -> stronger := List.concat [!stronger;x]; List.iter (
    fun y -> List.iter (
      fun z-> if (y <> z) then (
         Hashtbl.add sat_implies (y^">"^z) "y";
         (*print_string (y^">"^z^"\n")*))
    ) x
  ) !stronger
) equivalent_solvers

let do_formula_tree formula_tree =
          let normal_s = (format_tree formula_tree) ^ "\n" in
      	  append_s_to_fname_ [Open_append;Open_creat] normal_s ("log." ^ max_runtime);
          (*List.iter print_string (tree_leafs formula_tree);
          print_string ((format_tree (remap_leafs formula_tree)) ^ "\n");
          print_string ((format_tree2 formula_tree) ^ "\n");
          do_mlsolver formula_tree ;
           do_mark formula_tree*)
          _cache_result_sat := []; 
          _cache_result_unsat := []; 
          ignore (Do_parallel.do_commands (required_tasks formula_tree) max_runtime_float max_concurrent);
          List.iter ( fun y-> List.iter ( fun z-> 
                let rule=(y^">"^z) in
                  if (Hashtbl.mem sat_implies rule) then (
                            print_string ("RULE Failed: "^rule^"\n");
                             Mainlib.log (Mainlib.log_dir ^ "unify_BUG.log") (rule^" "^normal_s);
                  )
          ) !_cache_result_unsat ) !_cache_result_sat

          (*if verbose then List.iter (fun x ->
                       let (l,(regexp,title)) = x in
                         print_string title;
                         List.iter (fun s -> print_string (s ^ " ")) (!l);
                         print_string "\n"
          ) result_info *)

let test_rule_ rule = do_formula_tree {l="-"; c=[rule]}
                        (*
let test_rule_ rule = Printf.printf "STUB: Test rule %s\n" (format_tree rule)
                        *)
let test_rule t1 t2 = if (tree_length t2) < (tree_length t1) then test_rule_ {l="="; c=[t1;t2]}  

let rule_of_formula phi = match phi with {l="-"; c=[alpha; beta]} -> (alpha,beta) 
let formula_of_rule alpha beta = {l="-"; c=[alpha; beta]}

                                  
let rec squeeze ll = 
  if (List.hd ll) = [] then [] else List.concat [
                  List.map List.hd ll;
      squeeze     (List.map List.tl ll)
 ];;
    

(* let replace = Str.global_replace (Str.regexp_string find) replace_string *)

let simplify_rule t1_ t2_ =
      printf "Ximplify rule: %s :: %s\n" (format_tree t1_) (format_tree t2_);
  rule_found := true;
  let max_lhs = 9 in
  (*print_endline "AA";*)
  let rule = ref (t1_,t2_) in
  let rec r pt1 pt2 t = (
    if not (!rule_found || t.c = []) then 
    let pt = format_tree_prefix t in
      let regex = Str.regexp_string pt in
      let pt1b = Str.global_replace regex "z" pt1 in
      let pt2b = Str.global_replace regex "z" pt2 in
      let lt1b = String.length pt1b in
      let lt2b = String.length pt2b in
  (*print_endline "AX";*)
      if ((lt1b < max_lhs) && (lt2b < lt1b)) then (
          let t1b = parse_tree_prefix (Str.global_replace regex "z" pt1) in
          let t2b = parse_tree_prefix (Str.global_replace regex "z" pt2) in
          printf "  Is Simpler? %s :: %s\n" (format_tree t1b) (format_tree t2b);
          test_rule t1b t2b; 
          if (!rule_found) then print_endline "YES!";
          if (!rule_found) then rule := rule_of_formula (rename_variables (formula_of_rule t1b t2b))
        );
      List.iter (r pt1 pt2) t.c
  ) in
  while (!rule_found) do 
  rule_found := false;
    let t1, t2 = (!rule) in
      printf " Simplify rule: %s :: %s\n" (format_tree t1) (format_tree t2);
      (r (format_tree_prefix t1) (format_tree_prefix t2) t1)
  done;
  store_rules := true;
  let t1, t2 = (!rule) in
  rule_found := true;
  test_rule t1 t2 
;;


let find_rule t =
  rule_found := false;
  (* let found = ref false in 
  let rule = ref {l=""; c=[]} in *)
  let rec r subtree = (
    if not ((!rule_found) || Hashtbl.mem known_lhs (format_tree_prefix subtree)) then ( 
        List.iter r subtree.c;
        store_rules := true;
        if not (!rule_found) then test_rule subtree {l="0"; c=[]};
        if not (!rule_found) then test_rule subtree {l="1"; c=[]};
      if not (!rule_found) then (
        let rec rr simple = (
          if not (!rule_found) then (
            List.iter rr simple.c;
            store_rules := false;
            test_rule subtree simple;
              if (!rule_found) then simplify_rule subtree simple;
            store_rules := true;
          )
        ) in
        rr subtree
      );
      Hashtbl.replace known_lhs (format_tree_prefix subtree) ();
    )
  ) in
    r t;
   !rule_found

     (*
let find_rule__ t = 
  rule_found := false;
  test_rule t {l="0"; c=[]};
  if (!rule_found) then 
    true 
  else (
    test_rule t {l="1"; c=[]};
    if (!rule_found) then 
      true
    else find_rule_ t
  )

let find_rule t =
  let prefix_lhs = format_tree_prefix t in 
  if Hastbl.mem known_lhs prefix_lhs then 
    false 
  else (
    let ret = find_rule__ t in
    Hastbl.replace known_lhs prefix_lhs ();
    ret
  )
      *)
    
   

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
  let status = ref "bad" in (
    try (
(*      print_endline origcwd; *)
      let formula_tree = parse_ctls_formula s in
        print_string ("Input formula: " ^ (format_tree formula_tree) ^ "\n");
        let formula_tree = rename_variables formula_tree in 
        print_string ("Normalised to: " ^ (format_tree formula_tree) ^ "\n");
        print_string ("mlsolver fmt: " ^ (format_tree2 formula_tree) ^ "\n");
        print_string ("Force State Var: " ^ (format_tree (force_state_var formula_tree)) ^ "\n");
        let formula_tree = (if (!settings_simplify) 
		then simplify (!rule_list) (rename_variables formula_tree)
		else formula_tree) in
        (if (!settings_simplify) then print_string ("Simplified to: " ^ (format_tree formula_tree) ^ "\n"));
	print_endline ("ID: "^(canonical_file formula_tree));
	do_formula_tree formula_tree;
        if settings_do_negation then (
                let formula_tree = {l="-"; c=[formula_tree]} in 
                print_string ("Negation: " ^ (format_tree formula_tree) ^ "\n");
        	do_formula_tree formula_tree
        );
	status := "good"
    ) with
        Parsing.Parse_error-> print_string "Could not parse Formula.\n"
  );
  Mainlib.print_count "Program" "unify_stats.txt";
  Mainlib.log (Mainlib.log_dir ^ "unify_" ^ !status ^ ".log")
  (Mainlib.string_map (fun c->if c='\n' then ' ' else c) s)
;;

let do_benchmark s = (
  clear_result_info();
  let status = ref "bad" in
    (try (
(*      print_endline origcwd; *)
      let ft_ = parse_ctls_formula s in
        print_string ("Input formula: " ^ (format_tree ft_) ^ "\n");
        let ft = rename_variables ft_ in
       List.iter (fun (out_fname, tasks) ->
        let results = List.map ( fun (solver_name, t) ->
                let fname = "out/" ^ (canonical_file t) ^ "." ^ solver_name ^ "3600" in
                (process_file_stats solver_name fname t)) tasks in
        appendc_s_to_fname (String.concat " & " (("$"^format_tree_tex ft_^"$")::(squeeze results)) ^ "\\\\ \n") out_fname 
       ) [
           ("out/benchmark.tex",    [("BPATH"  , ft); ("BPATHf"  , ft); ("BPATH"  , force_state_var_A ft); ("BCTLNEW", ft)]);
           ("out/benchmarkhue.tex", [("BPATHUE", ft); ("BPATHUEf", ft); ("BPATHUE", force_state_var_A ft); ("BCTLHUE", ft)])
          ] 
    ) with
        Parsing.Parse_error-> print_string "Could not parse Formula.\n")
                                ;
                              Mainlib.print_count "Program" "unify_stats.txt";
                              Mainlib.log (Mainlib.log_dir ^ "unify_" ^ !status ^ ".log")
                                (Mainlib.string_map (fun c->if c='\n' then ' ' else c) s)
)

(*let do_string__ s = do_string_ s ; do_string_ ("-(" ^ s ^ ")")*)

(* Benchmark Stuff *)
(*
let b_process_file name fname t =
  if verbose then Printf.printf "**** Begin %s\n" name;
  if (Sys.file_exists fname) then (
    let chan = open_in fname in
    let size = 80 * 1024 in
    let buffer = String.create size in
    let len = input chan buffer 0 size in
    close_in_noerr chan;
    let s = (String.sub buffer 0 len) in
    let runtime_s = try
      let r = Str.regexp "RUNTIME: \\([0-9.]*\\)" in
        ignore(Str.search_forward r s 0);
        let time_s = Str.matched_group 1 s in
        Printf.sprintf "%0.2f" (float_of_string time_s)
    with Not_found -> "?.??" in
    if verbose then print_string (s);
    if (len >= size) then print_string "Data file too long\n";
    List.iter (fun x ->
                 let (l,(regexp,title)) = x in
                 let result_str = (name^runtime_s) in
                 try ignore(Str.search_forward regexp s 0) ;
                     (match (title, name, t) with
                         (* "  UNsatisfiable: "title_unsat_str, "BPATH", {l="-";
                          * c=[rule]}) -> *)
                         ( "  UNsatisfiable: ", ("BPATH" | "BPATHUE"), {l="-"; c=[rule]}) -> if (rule.l="=") then add_rule rule;
                           (*if (title == title_unsat_str) then (
                           Printf.printf "Title is %s\n" title ; if !store_rules then add_rule rule *)
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
 *)

let expect_file fname = 
  if not (Sys.file_exists fname) then (
    Sys.command("pwd");();
    print_endline ("Warning, cannot find file: "^fname)
  )
   

let main () =
  print_endline ("Data/current dir: "^(Sys.getcwd()));
  expect_file "mark/JApplet.class";
  expect_file "mlsolver/bin/mlsolver";
  print_string "main loop";

  Sys.set_signal Sys.sigfpe
      (Sys.Signal_handle (fun _ -> print_string "blush\n" ; Printexc.print_backtrace stdout ; print_string "flush\n" ; flush stderr ; failwith "FPE")); 
  try 
    while true do
      try
        print_string "\n# ";
        let line = try read_line() with End_of_file -> (print_string "End of input\n" ; flush stdout; exit 0)  in
          print_string (line ^ "\n"); flush stdout;
	  match line with
	    "PRINT_TRS_TEX" -> print_rules_tex (List.sort compare_rule (!rule_list_TRS))
            | _ -> match (line^"#").[0] with 
                'R' -> cat rule_fname
              | '<' -> settings_simplify := false
              | '>' -> settings_simplify := true 
              | 'S' -> do_simplify simplify_star  (split_at_n_r line 1)
              | ',' -> do_simplify simplify_star_TRS  (split_at_n_r line 1)
              | '.' -> do_simplify simplify_star_both  (split_at_n_r line 1)
              | 'L' -> do_simplify simplify_learn (split_at_n_r line 1)
              | 'B' -> do_benchmark (split_at_n_r line 1)
              | 'T' -> print_string (dump_TRS())
              | '#' -> ()
              | '*' -> settings_solvers := ["*"]
              | _ -> do_string (line)
      with
          Parsing.Parse_error -> Printf.printf "Parse Error!\n"
        | Not_found -> print_string "Not_Found Error XYZ" 
    done;
  with 
       | End_of_file -> (print_string "End of input??\n" ; flush stdout; exit 0) 
       | x -> print_string (Printexc.get_backtrace ()) ; print_string (Printexc.to_string x); failwith "Unexpected exception 1"

let _ =
  Printf.printf "Content-Type: text/plain\n\n";
  (*let (p,t) = parse_tree_trs_ 0 "U(a,b)" in
  print_endline (format_tree t);*)
  let _ =  parse_tree_TRS_rule_ " U(implies(x, y), -(y)) -> F(-(y))" in
  try
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


