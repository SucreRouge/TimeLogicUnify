(* file: mainlib.ml 
 *
 * The is the main file used to do model checking. It is essentially a wrapper
 * around the model-checker found in me.ml *)

let origcwd = Sys.getcwd ()
(*let home = try Sys.getenv "HOME" with _ -> origcwd^"/../.."*)
let publichtml = origcwd^"/../"

let printf=Printf.printf

let handle_error s f x =
try f x
with e -> Printf.printf "%s> %s\n" s (Printexc.to_string e); flush stdout; raise e
	
let clear_equals = fun s -> let p = String.index s '=' in String.sub s (p+1) (String.length s - p - 1)
let rec fixstr_ s i j = if j >= String.length s then String.sub s 0 i else let (si,jj)=(match s.[j] with '%' -> (char_of_int (int_of_string ("0x"^(String.sub s (j+1) 2))) ,j+3) | '+' -> (' ',j+1) | _ -> (s.[j],j+1)) in s.[i]<-si ; fixstr_ s (i+1) jj 
let fixstr = fun s -> fixstr_ (clear_equals s) 0 0

let split_string s =
	let ind = String.index s ':' in 
	(String.sub s 0 ind, String.sub s (ind+1) ((String.length s)-ind-1))

let newline_split s ind =
	String.sub s 0 ind, String.sub s (ind) ((String.length s)-ind)

let append_char s c = s ^ (String.make 1 c)

let get_url_argument key s =
      let r = Str.regexp (key^"=\\([^&#]*\\)") in
        let _ = Str.search_forward r s 0 in
        fixstr_ (Str.matched_group 1 s) 0 0

let get_url_list key s =
      let r = Str.regexp (key^"=\\([^&#]*\\)") in
      let rec f pos =  
        try 
		let next = 1 + (Str.search_forward r s pos) in
		let str = fixstr_ (Str.matched_group 1 s) 0 0 in
		str::(f next)
	with Not_found -> [] in
	handle_error "LIST" f 0  

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

let log_dir=publichtml^".data/" 
	(* FIX !!!*)
	(*(Sys.getenv "HOME") ^ "/.config/"
with Not_found -> 
	let cwd = Sys.getcwd () in 
	if ((String.sub cwd 0 5)="/home") 
		then cwd^"/../../.config/"
		else "/var/www/.config/"
	*)

let print_count name file = 
        let stats_fname = log_dir ^ file in
	Printf.printf "\nThis %s has been used %s times\n" name
(try 
        let count = try let old_count =
        	let input = open_in stats_fname in
		let cnt = input_line input in 
		close_in input;
		cnt in
		string_of_int (1+ int_of_string (old_count))
	with _ -> (print_endline ("Cannot Access: "^stats_fname); "1") in
        let output = open_out stats_fname in
        output_string output count;
        close_out output;
        count
with _ -> (print_endline ("Cannot Write: "^stats_fname); "UNKNOWN"))
        
let log f_name s =
        let oc = open_out_gen [Open_creat; Open_text; Open_append] 0o600 f_name
        in
          output_string oc (s^"\n");
            close_out oc  

let robust_parse_formula p = robust_parse [split_every_char; v_to_or] p Phi_lexer.token 

let check_lang lang_name bad_ops f = List.iter ( fun op -> 
       let c = String.get op 0 in
       if (String.contains f c) then (
              printf "Formula contains %c but %s is not allowed in %s\n" c op lang_name;
              raise Parsing.Parse_error
       )
 ) bad_ops 
let parse_ctls_formula f = check_lang "CTL*" ["Since"]                      f ; robust_parse_formula Phi_parser.ifixs f
let parse_rtl_formula  f = check_lang "RTL"  ["All Paths"; "Exists a Path"] f ; robust_parse_formula Phi_parser.formula f
