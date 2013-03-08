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

let origcwd = Sys.getcwd ()

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



let is_sat_regexp = Str.regexp_case_fold "is satisfiable";;
let is_sat_regexp = Str.regexp " unsatisfiable"
let result_parse_info = [ ( Str.regexp_case_fold "is satisfiable", "Satisfiable: ");
                          ( Str.regexp " unsatisfiable",           "UNsatisfiable: ") ]
let add_ref_list a : string list ref * 'a = (ref [],a)
let result_info = List.map add_ref_list result_parse_info
let clear_result_info () = List.iter (fun e -> (fst e) := []) result_info
(*let result_info = List.map (fun a -> (ref [],a)) result_parse_info
*)(*let result_info = [ ( Str.regexp_case_fold "is satisfiable", "Satisfiable: "  , ref[]);
                    ( Str.regexp " unsatisfiable",           "UNsatisfiable: ", ref[]) ] *)

(* From: http://www2.lib.uchicago.edu/keith/ocaml-class/complete.html *)
let process_file name fname =
  Printf.printf "**** Begin %s\n" fname;
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
    print_string (s);
    if (len >= size) then print_string "Data file too long\n";
    List.iter (fun x ->
                 let (l,(regexp,title)) = x in
                 try ignore(Str.search_forward regexp s 0) ; l := (name^runtime_s)::(!l)
                 with Not_found -> ()
    ) result_info
  ) else (
    print_string (fname ^ " does not exist\n")
  );
  Printf.printf "**** End %s\n" fname;
  ()

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

let remap_leafs_ m t =
  let leafs = tree_leafs t in
    if List.length leafs > 26 then failwith "Cannot map more than 26 variables to letters";
    let a = Array.make 26 "" in
    let idx s = m * (Char.code (String.get s 0) - Char.code 'a') in
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

let append_s_to_fname s fname =
  let f = open_out_gen [Open_append] 0o644 fname in
    output_string f s;
    close_out f


let java_entry name = ( name, "mark/",  fun t fname ->
                          redirect_output "/dev/null";
                          Unix.chdir "mark/";
                          let args =
                            [| "java"; "-Djava.awt.headless=true"; "JApplet";
                               format_tree_mark t; name ; fname |] in
                            (*Unix.execvp "echo" args;*)
                            Unix.execvp "java" args )

let mlsolver_entry = ( "mlsolver", "", fun t fname ->
                         redirect_output fname;
                         Unix.execv "mlsolver/bin/mlsolver"
                           [| "mlsolver"; "-pgs"; "recursive";
                              "-ve"; "-sat"; "ctlstar"; format_tree2 t  |]
)

(* gives the canonical file name for a tree t: STUB *)
let md5 s = Digest.to_hex (Digest.string s)
let canonical_file t = md5 (format_tree_mark t)

let required_tasks t =
  let solver_entries = [java_entry "BCTLNEW" ; mlsolver_entry] in
  let solver_entries = [mlsolver_entry; java_entry "BCTLNEW"; java_entry
                                                                "BCTLOLD" ; java_entry "CTL" ] in
  let tasks = ref [] in
    List.iter  ( fun e ->
                   let (solver_name, prefix, f) = e in
                   let fname = "out/" ^ (canonical_file t) ^ "." ^ solver_name in
                   let fullfname = prefix ^ fname in
                   let task_f = (fun () -> f t fname) in
                   let on_finish_f1 = (fun () -> process_file solver_name fullfname ) in
                     if (Sys.file_exists fullfname) then
                       on_finish_f1 ()
                     else
                       let on_finish_f runtime = (append_s_to_fname ("\nRUNTIME: "^(string_of_float runtime)^"\n") fullfname;
                       on_finish_f1 ()) in
                       tasks := (task_f, on_finish_f)::(!tasks)
    ) solver_entries ;
    Array.of_list(!tasks)

(*if not Sys.file_exists *)





(*let do_mlsolver t = ignore (Do_parallel.do_commands (fun () ->  [|
 [| "../../4mlsolver/mlsolver/bin/mlsolver.exe"; "-pgs"; "recursive";
 "-ve"; "-val"; "ctlstar"; format_tree2 t  |]
 |] 1.9 3)  *)

let do_string s =
  clear_result_info();
  let status = ref "bad" in
    try
      let formula_tree = parse_ctls_formula s in
        print_string ("Input formula: " ^ (format_tree formula_tree) ^ "\n");
        let formula_tree = rename_variables formula_tree in
          print_string ("Normalised to: " ^ (format_tree formula_tree) ^ "\n");

          List.iter print_string (tree_leafs formula_tree);
          print_string ((format_tree (remap_leafs formula_tree)) ^ "\n");
          print_string ((format_tree2 formula_tree) ^ "\n");
          (*do_mlsolver formula_tree ;
           do_mark formula_tree*)
          ignore (Do_parallel.do_commands (required_tasks formula_tree) 1.9 3);

          List.iter (fun x ->
                       let (l,(regexp,title)) = x in
                         print_string title;
                         List.iter (fun s -> print_string (s ^ " ")) (!l);
                         print_string "\n"
          ) result_info

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

