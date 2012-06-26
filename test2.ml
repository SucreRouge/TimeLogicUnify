#!/usr/bin/env ocaml
(* boilerplate *)


let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;

(*let explodeS s = List.Map Char.escaped explode s;;*)
let explodeS s = List.map Char.escaped (explode s);;  

module StringSet=Set.Make(String);;
type label = LabelC of char | LabelS of StringSet.t;;
type tree = { l : label; c : tree list; };;
let x = {l=LabelC('x'); c=[{l=LabelC('z');c=[]}]};;
let rec tree_iter t f = {l=f(t.l); c=List.map (function tt -> tree_iter tt f) t.c};;
let emptyLetter = StringSet.add "1" StringSet.empty;;
let stringset_of_list li =      
    List.fold_left (fun set elem -> StringSet.add elem set) StringSet.empty li
let letter_of_list li =      
    List.fold_left (fun set elem -> StringSet.add elem set) emptyLetter li
let letter s = letter_of_list (explodeS s)   ;;


let ttt = {l=LabelC 'x'; c=[{l=LabelS(letter "pq");c=[]}]};;
let ttt2 = {l=LabelC '+'; c=[{l=LabelS(letter "p");c=[]};{l=LabelS(letter "q");c=[]}]};;



StringSet.elements emptyLetter;;

let rec apply_if_letter ll f = match ll with LabelC l -> LabelC l | LabelS s -> LabelS (f s);;

let rec tree_iter t f = {l=f(t.l); c=List.map (function tt -> tree_iter tt f) t.c};;
let rec tree_iterL tt f = tree_iter tt (function ll -> apply_if_letter ll f);;

let rec tree_and t p q = tree_iterL t (function s -> if StringSet.mem p s && StringSet.mem q s then StringSet.add (p ^ "&" ^ q) s else s);;

let stringset_to_string sl = "{" ^ String.concat ", " (StringSet.elements sl) ^ "}";;
let rec tree_to_string t = match t.l with LabelC l -> (Char.escaped l ^ "(" ^ (String.concat ", " (List.map tree_to_string t.c)) ^ ")") | LabelS s -> stringset_to_string s;;

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
(* let rec all_leaves_contain s t = not (a_leaf_satisfies (function set -> not (StringSet.mem s set)) t);; *)
let rec all_leaves_contain s t = a_leaf_satisfies (function set -> StringSet.mem s set) t;;
let rec all_leaves_contain_p_or_q p q t = a_leaf_satisfies (function set -> StringSet.mem p set || StringSet.mem q set) t;;
(*  satisfy (function set -> StringSet.mem q (leftmost t) || StringSet.mem ;;
*)
let pUq_from_pq p q = "(" ^ p ^ "U" ^ q ^ ")";;
let leftmost_sat_pUq p q t = all_leaves_contain_p_or_q (pUq_from_pq p q) q t;;
let leftmost_sat_pUq p q tt = let t = (leftmost tt) in 
	let pUq = (pUq_from_pq p q) in
	match t.l with
		LabelC l -> all_leaves_contain p t && a_leaf_contains pUq t
		| LabelS s -> StringSet.mem q s || ( StringSet.mem pUq s && StringSet.mem p s );;

(*let leftmost_sat_pUq p q t = all_leaves_satisfy (function set -> StringSet.mem q (leftmost t) || StringSet.mem ;;
*)

let errorTree={l=LabelC '?';c=[]};;

let rec tree_until b t p q = 
	let pUq = (pUq_from_pq p q) in
	let sat_pUq = (leftmost_sat_pUq p q)  in
	match t.l with
        	LabelC l -> ( match l with 
			  '+' -> let right_tree = ( tree_until b (List.hd (List.tl t.c)) p q ) in
				let left_tree = tree_until (sat_pUq right_tree) (List.hd t.c) p q  in
				{l=t.l;c=[left_tree;right_tree]}
			| '>' -> let dummy_node = tree_until b (List.hd t.c) p q in
				let b2 = sat_pUq (dummy_node) in
				if (b == b2) then {l=t.l;c=[dummy_node]} else tree_until b2 (List.hd t.c) p q
			| '<' -> let right_tree = ( tree_until b (List.hd t.c) p q ) in
				let left_tree = tree_until (sat_pUq right_tree) (List.hd t.c) p q  in
				{l=LabelC '+';c=[{l=LabelC '<';c=[left_tree]}; right_tree]}
			| 'S' -> let b2 = (b && all_leaves_contain p t) || (all_leaves_contain p t && all_leaves_contain q t) in
				{l=t.l;c=List.map (function tt -> tree_until b2 tt p q) t.c} 
                        |  _  -> errorTree  )
		| LabelS s -> if b then {l=LabelS (StringSet.add pUq s); c=[]} else t;;

let print_bool b = print_string ( string_of_bool b );;

print_string ( tree_to_string ttt );;
print_bool (a_leaf_contains "p" ttt);;
print_bool (a_leaf_contains "z" ttt);;
print_bool (all_leaves_contain "p" ttt);;
print_bool (all_leaves_contain "z" ttt);;
print_bool (all_leaves_contain "p" {l=LabelC 'x';c=[]});;
print_bool (a_leaf_contains "p" {l=LabelC 'x';c=[]});;

print_string ( tree_to_string (tree_until false ttt2 "p" "q"));;
