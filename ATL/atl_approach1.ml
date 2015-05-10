(* ocaml can be quite terse, but still wants us to define integers
   as modules before defining sets
   
   There is no real mathematical meaning behind this *)
   
module IntInt = struct
  type t = int * int
  let compare = compare
end

module Int = struct
  type t = int
  let compare = compare
end

module IntSet = Set.Make(Int);;
module IntRelation = Set.Make(IntInt);;

(*
let generic_map fold add empty f s =
    let work x r = add (f x) r in
    fold work s empty
  ;;
  
let intset_map = generic_map IntSet.fold IntSet.add IntSet.empty 
*)
(*
module PowerSet(S: Set.S) =
struct
 
  include Set.Make (S)

  let map f s =
    let work x r = add (f x) r in
    fold work s empty
  ;;

  let powerset s = 
    let base = singleton (S.empty) in
    let work x r = union r (map (S.add x) r) in 
    S.fold work s base
    ;;
 
end;; (* PowerSet *)


*)

let ( ==> ) a b = ((!a) || b)

(* Now we define a tree in the obvious way *)

type 'a tree = {l: 'a; c: 'a tree list};;

(* A formula can be thought of as a labelled tree *)

type label =
    | Op of char
    | Agents of IntSet.t;;

type formula = label tree;;
    
let label2string l = match l with 
	| Op (c) -> String.make 1 c
	| Agents (a) -> "{" ^ (String.concat "" (List.map string_of_int (IntSet.elements a))) ^"}" ;;
    
let rec tree2string l2s t =
	let t2s = (tree2string l2s) in
	match t with
		| {l=l; c=[]} -> l2s l
		| {l=l; c=[x]} -> (l2s l) ^ (t2s x)
		| {l=l; c=[x;y]} ->  "(" ^ (t2s x) ^ (l2s l) ^ (t2s y) ^ ")" 
		| _ -> "Error(tree2string)";;
		
let formula2string = (tree2string label2string);;
    
module Formula = struct
	type t = formula
	let compare=compare
	let to_string=(tree2string label2string)
	let print x = (print_string (to_string x))
	let println x = print_string ((to_string x) ^ "\n") 
end

(*Now fix some formula phi to decide*)

let phi = {l=Op 'q'; c=[]};;
let phi = {l=Op '~'; c=[phi]};;
let phi = {l=Op 'X'; c=[phi]};;
let phi = {l=Agents (IntSet.singleton 1); c=[phi]};;

let list_subsets xs = List.fold_right (fun x rest -> rest @ List.map (fun ys -> x::ys) rest) xs [[]]

module Hue = struct
	include Set.Make(Formula)
	let bigunion = List.fold_left union empty
	
	let rec closure_of p = 
		let cld = add p (
				bigunion (List.map closure_of p.c) 
			) in 
		match p with 
		| {l=Agents a; c=c} -> let x=[{l=Agents a; c=c}] in
			add {l=Op '~'; c=x} (
			add {l=Op 'V'; c=x} (			
			add {l=Op 'v'; c=x} (			
			cld)))
		| {l=Op '~'; c=c} -> cld
		| x -> add {l=Op '~'; c=[x]} cld
(*	
	let rec closure_of p = 
		add {l=Op '~'; c=[p]} (
		add p (
			bigunion (List.map closure_of p.c) 
		)
	)
*)
	
	let closure = closure_of phi;;

	let _ = iter Formula.println closure;; 

	let mpc h = for_all (fun b -> let has x = mem x h in 
					match b with
					| {l=Op '~'; c=[a]} ->   ( (has b) != (has a) )
					| {l=Op '&'; c=[x;y]} -> ( (has b)  = ((has x) && (has y)) )
					| _ -> true
				) closure;;
	
	let valid h = (mpc h) && 
				  for_all (fun p ->
				    let has x = mem x h in
					match p with 
					| {l=Op 'U';c=[a,b]} -> (has a) || (has b)
					| {l=Op '~';c=[
						{l=Op 'U';c=[a,b]}
					  ]} -> (!(has b))
					| {l=Agents a   
					
							
(*
	let mpc h = for_all (fun b -> let has = mem h in
					match b with
					| {l=Op '~'; c=[a]} ->   ( (has b) != (has a) )
					| {l=Op '&'; c=[x;y]} -> ( (has b)  = ((has x) && (has y)) )
					| _ -> true
				) closure;;
	
	let valid h = (mpc h)
				
*)			  
end;;
(*
module PowerSetofHue=PowerSet(Hue);;
*)

module Colour = Set.Make(Hue);;

(*

let x = Op 'x';;
 
let _ = match x with 
	| Op (c) -> print_char c
	| Agents (a) -> print_char '\n';;
	
let t = {l=Op 'p'; c=[]};;

print_string ("X" ^ (formula2string t) ^"Y\n");


let rec hue_union l =
	match l with 
		| [] -> Hue.empty
		| head::tail -> Hue.union head (hue_union tail);;
*)


(*	

let rec closure_of p = 
	let cld = Hue.add p (
			hue_bigunion (List.map closure_of p.c) 
		) in 
	match p with 
	| {l=Agents a; c=c} -> Hue.add {l=Agents a; c=c} (
		Hue.add {l=Op '~'; c={l=Agents a; c=c}} cld)
	| {l=Op '~'; c=c} -> cld
	| x -> Hue.add {l=Op '~'; c=[x]} cld
	
	
		
	Hue.add {l=Op '~'; c=[p]} (
		
	);;
	
let closure = closure_of phi;;

let hues = PowerSetofHue.powerset closure;;


let r=Hue.empty
	Hue.empty.add(p).add({l="~"; c=[p]});;
	Hue.add (
		{l="~"; c=p}
		(Hue.add p Hue.empty));;

*)






(*
let _ = match x with 
	| Atom (c) -> print_char c
	| Agents (a) -> print_char '\n'
	| And -> print_char '&' ;;


type label =
	| And | Neg
	| Until | Next
    | Atom of char
    | Agents of IntSet.t;;
    
print_char x.Op;;

type label = | Atom of char | Foo of char;;

type label = And | Neg | Atom of char | Until | Next | Agents of IntSet
*)





