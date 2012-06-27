#!/usr/bin/env ocaml

(* Some preliminary definitions *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;
let explodeS s = List.map Char.escaped (explode s);;  
let print_bool b = print_string ( string_of_bool b );;
module StringSet=Set.Make(String);;

(* Now we start our mathematical definitions *)
type label = LabelC of char | LabelS of StringSet.t;;
type tree = { l : label; c : tree list; };;
type ftree = { op : char; ch : ftree list; };;
let x = {l=LabelC('x'); c=[{l=LabelC('z');c=[]}]};;
let rec tree_iter t f = {l=f(t.l); c=List.map (function tt -> tree_iter tt f) t.c};;
let emptyLetter = StringSet.add "1" StringSet.empty;;
let stringset_of_list li =      
    List.fold_left (fun set elem -> StringSet.add elem set) StringSet.empty li
let letter_of_list li =      
    List.fold_left (fun set elem -> StringSet.add elem set) emptyLetter li
let letter s = letter_of_list (explodeS s)   ;;

StringSet.elements emptyLetter;;

let rec apply_if_letter ll f = match ll with LabelC l -> LabelC l | LabelS s -> LabelS (f s);;

let rec tree_iter t f = {l=f(t.l); c=List.map (function tt -> tree_iter tt f) t.c};;
let rec tree_iterL tt f = tree_iter tt (function ll -> apply_if_letter ll f);;

let stringset_to_string sl = "{" ^ String.concat ", " (StringSet.elements sl) ^ "}";;
let rec tree_to_string t = match t.l with LabelC l -> (Char.escaped l ^ "(" ^ (String.concat ", " (List.map tree_to_string t.c)) ^ ")") | LabelS s -> stringset_to_string (StringSet.remove "1" s);;
let println_tree t = print_endline (tree_to_string t);;

let rec leftmost t = match t.l with 
	LabelC l -> ( match l with '+' -> leftmost (List.hd t.c) 
	                         | '>' -> leftmost (List.hd t.c) 
	                         |  _  -> t )
	| LabelS s -> t;;

let rec a_leaf_satisfies f t = match t.l with
        LabelC l -> List.exists (function tt -> a_leaf_satisfies f tt) t.c
        | LabelS s -> f s;;

let rec all_leaves_satisfy f t = not (a_leaf_satisfies (function set -> not (f set)) t);;

let rec a_leaf_contains s t = a_leaf_satisfies (function set -> StringSet.mem s set) t;;
let rec all_leaves_contain s t = a_leaf_satisfies (function set -> StringSet.mem s set) t;;
let rec all_leaves_contain_p_or_q p q t = a_leaf_satisfies (function set -> StringSet.mem p set || StringSet.mem q set) t;;
let pUq_from_pq p q = "(" ^ p ^ "U" ^ q ^ ")";;
let pSq_from_pq p q = "(" ^ p ^ "S" ^ q ^ ")";;
let leftmost_sat_pUq p q t = all_leaves_contain_p_or_q (pUq_from_pq p q) q t;;
let leftmost_sat_pUq p q tt = let t = (leftmost tt) in 
	let pUq = (pUq_from_pq p q) in
	match t.l with
		LabelC l -> all_leaves_contain p t && a_leaf_contains pUq t
		| LabelS s -> StringSet.mem q s || ( StringSet.mem pUq s && StringSet.mem p s );;

let errorTree={l=LabelC '?';c=[]};;

let rec tree_until_ b t p q pUq = 
	let sat_pUq = (leftmost_sat_pUq p q)  in
	match t.l with
        	LabelC l -> ( match l with 
			  '+' -> let right_tree = ( tree_until_ b (List.hd (List.tl t.c)) p q pUq ) in
				let left_tree = tree_until_ (sat_pUq right_tree) (List.hd t.c) p q pUq  in
				{l=t.l;c=[left_tree;right_tree]}
			| '>' -> let dummy_node = tree_until_ b (List.hd t.c) p q pUq in
				let b2 = sat_pUq (dummy_node) in
				if (b == b2) then {l=t.l;c=[dummy_node]} else tree_until_ b2 (List.hd t.c) p q pUq
			| '<' -> let right_tree = ( tree_until_ b (List.hd t.c) p q pUq) in
				let left_tree = tree_until_ (sat_pUq right_tree) (List.hd t.c) p q pUq in
				{l=LabelC '+';c=[{l=LabelC '<';c=[left_tree]}; right_tree]}
			| 'S' -> let b2 = (b && all_leaves_contain p t) || (all_leaves_contain p t && a_leaf_contains q t) in
				{l=t.l;c=List.map (function tt -> tree_until_ b2 tt p q pUq) t.c} 
                        |  _  -> errorTree  )
		| LabelS s -> if b then {l=LabelS (StringSet.add pUq s); c=[]} else t;;

let rec mirror t = let c2 = List.rev t.c in
	match t.l with 
	LabelC x -> { l = LabelC (match x with '>' -> '<' | '<' -> '>' | _ -> x); c=c2 }
	| LabelS s -> t;;

let tree_until t p q =
	let pUq = (pUq_from_pq p q) in
	tree_until_ false t p q pUq;;
let rec tree_and t p q = tree_iterL t (function s -> if StringSet.mem p s && StringSet.mem q s then StringSet.add (p ^ "&" ^ q) s else s);;
let rec tree_not t p = tree_iterL t (function s -> if StringSet.mem p s then s else StringSet.add ("-" ^ p) s);;
let tree_since t p q =  
	let pSq = (pSq_from_pq p q) in
	mirror ( tree_until_ false (mirror t) p q pSq );;

let rec string_from_ftree t = match List.length t.ch with
	0 -> Char.escaped t.op
	| 1 -> Char.escaped t.op ^ string_from_ftree (List.hd t.ch)
	| 2 ->  "(" ^  string_from_ftree (List.nth t.ch 0) ^ (Char.escaped t.op) ^ string_from_ftree (List.nth t.ch 1) ^ ")"
	| _ -> " ERROR!!! ";;

let rec model_check_ me ft  = 
	let me2 = (List.fold_left model_check_ me ft.ch) in
	let ch_strings = (List.map string_from_ftree ft.ch) in
	let new_me = match ft.op with
		  'S' -> ( tree_since me2 (List.hd ch_strings) (List.nth ch_strings 1) )
		| 'U' -> ( tree_until me2 (List.hd ch_strings) (List.nth ch_strings 1) )
		| '&' -> ( tree_and me2 (List.hd ch_strings) (List.nth ch_strings 1) )
		| '-' -> ( tree_not me2 (List.hd ch_strings) )
		| _ -> me in
	if (new_me = me) then () else (print_string "... " ; println_tree (new_me));
	new_me;;

let model_check me ft =
	println_tree me;
	let me2 = model_check_ me ft in
	let ft_str = (string_from_ftree ft) in
	let sat = a_leaf_contains ft_str me2 in
	print_string (ft_str ^ (if sat then "" else " NOT") ^ " satisfied in ") ;
	println_tree me;
	sat;;

let test_me = {l=LabelC '+'; c=[{l=LabelS(letter "p");c=[]};{l=LabelS(letter "p");c=[]}]};;
let test_ft = {op='-';ch=[{op='U'; ch=[{op='p';ch=[]};{op='-';ch=[{op='q';ch=[]}]}]}]};;

model_check test_me test_ft;;
model_check (List.hd test_me.c) test_ft;;
model_check (List.hd test_me.c) (List.hd test_ft.ch);;
