(* === BoilerPlate === *)
(* ex: set tabstop=4 *)

(* \setcounter{section}{7}
 * \section{Appendix: An OCaml implementation for (NL-)BATL*} 
 * We shall begin with some basic definitions
 * *)

(*i*)
module Int = struct
  type t = int
  let compare = compare
end

module IntSet = struct
	include Set.Make(Int)
	let rec of_list l = match l with [] -> empty | h::t -> add h (of_list t) 
	let disjoint x y = equal (inter x y) empty
	let bigunion = List.fold_left union empty
end

module ISS = struct
	include Set.Make(IntSet)
	let rec of_list l = match l with [] -> empty | h::t -> add h (of_list t)
	let of_list2 ll = of_list (List.map IntSet.of_list ll)
	let union_all iss = IntSet.bigunion (elements iss)
	(* Agents are sets of integers. We now define what it means for a list of sets of agents to be disjoint *)
	let all_disjoint al = 
		let rec r prev l =
			match l with
			| [] -> true
			| head::tail -> 
				if IntSet.disjoint prev head
				then r (IntSet.union prev head) tail 
				else false in
		r IntSet.empty (elements al);;
	
end

let printf = Printf.printf;;
let sprintf = Printf.sprintf;;

let bool2str b = if b then "Y" else "n"

let println_list_of_int li = printf "[%s]\n" (String.concat "; " (List.map string_of_int li))

let rec range i j = if i > j then [] else i :: (range (i+1) j)
(*i*)



(* We sometimes want a list of subsets of a list, but that can be rather large. 
   So instead of building the powerset, we will sometime iterate over it instead.
   Also, it can be useful to iterate only over small subsets of size n or less
   so that we only have to deal with roughly $O(m^n)$ rather than $O(2^m)$ subsets *)

let subsets xs = List.fold_right (fun x rest -> rest @ List.map (fun ys -> x::ys) rest) xs [[]]

let iter_small_subsets f n xs =
	let rec r pl n xs =
		if (n < 1)
		then f pl
		else match xs with 
			| [] -> f pl
			| head::tail -> 
				r (head::pl) (n-1) tail;
				r pl          n    tail in
	r [] n xs;;

(*i*)
let iter_small_subsets_filt filt f n xs =
	let rec r pl n xs =
		if (filt pl)
		then ( 
			if (n < 1)
			then f pl
			else 
				match xs with 
				| [] -> f pl
				| head::tail -> 
					r (head::pl) (n-1) tail;
					r pl          n    tail 
		) in
	r [] n xs;;
(*i*)

(* === Standard-Maths === *)
(* We define $\Longrightarrow$ as one would expect. Note that this is not a formula. *)

let ( ==> ) a b = ((not a) || b)

(* We define fixpoints in the obvious way 
 * We test that our definition works correctly with an ``assert''
 * Clearly any natural number will converge to 0 when halved (rounding down)*)
let rec fixpoint f x = let fx = f x in
	if (x = fx) 
	then x
	else fixpoint f fx;;
	
assert ( (fixpoint (fun x -> x/2) 9) = 0);;

(* WARNING!!! The above function uses OCaml '='
         OCaml '=' is surprising to mathematicians. E.g. 
	{1,2} = {2,1} is FALSE, you want
	Set.equals {1,2} (2,1}, which is TRUE
*)

(* === Coalition === *)
(* A coalition is a set of agents, represented by integers *)
type coalition = IntSet.t

let coalition_to_string c = "{" ^ (String.concat "" (List.map string_of_int (IntSet.elements c))) ^ "}"


(* === Formula === *)
type formula =
	| ATOM of char
	| NOT of formula
	| AND of formula * formula   
	| NEXT of formula
	| UNTIL of formula * formula
	| ALLPATH of formula 
	| FALSE

(* === Abbreviation === *)
module Formula = struct
	type t = formula 
	let rec to_string psi = let s = to_string in
		match psi with
		| ATOM c      -> (String.make 1 c)
		| NOT x       -> "~" ^ (s x)
		| NEXT x      -> "X" ^ (s x)
		| AND (x,y)   -> "(" ^ (s x) ^ "&" ^ (s y) ^ ")"
		| UNTIL (x,y) -> "(" ^ (s x) ^ "U" ^ (s y) ^ ")"		
		| ALLPATH x   -> "A" ^ (s x)
		| FALSE -> "0" ;; (* The paper doesn't use FALSE, but it sure is convienient *)
	
	let of_string s =
		let i = ref 0 in
		let s = "("^s^")" in
		let rec c() = (
			let got=s.[(!i)] in
			i:=(!i)+1;
			print_char got;
			flush stdout;
			if got = ' ' || got = '?' then c() else got
		) in
		let rec r()=
			let rec bimodal x = 
				let op = c() in
				if op = ')' then x
				else bimodal (
					match op with 
					| '&' | '^' -> AND   (x,r())
					| 'U' -> UNTIL (x,r())
					| '|' -> NOT   (AND (NOT x, NOT (r())))
					| '>' -> NOT   (AND (    x, NOT (r())))
					| '=' -> let y = r() in 
						let tt =  AND (x,y) in
						let ff =  AND (NOT x, NOT y) in
						NOT (AND (NOT tt, NOT ff)) (* This may be a lot more efficient to reason about directly ... *)
					| x -> (let _ = printf "Unexpected op  %c at position %d" x (!i) in assert (false))
				) 
			in
			match (c()) with
			| '~' | '-' -> NOT (r())  
			| 'X' | 'N' -> NEXT (r())  
			| 'F' ->  UNTIL (NOT FALSE, r())  
			| 'G' ->  NOT (UNTIL (NOT FALSE, NOT(r())) )
			| 'A' ->  ALLPATH (r())  
			| 'E' ->  NOT (ALLPATH (r()))
			| '(' ->  bimodal (r()) 
			| '0' -> FALSE
			|  x  -> ATOM x in
		r();;

	let print x = (print_string (to_string x))
	let println x = print_string ((to_string x) ^ "\n") 
	let compare = compare
end  

let neg psi = 
	match psi with
	| NOT alpha -> alpha
	| alpha -> NOT alpha
	
(* === FixFormula === *)
(* We now fix a formula that we wish to decide *)
(*
let phi = NEXT (AND (ATOM 'p', NOT (ATOM 'p'))) 
*)

let phi =
	if Array.length Sys.argv > 1
	then Formula.of_string Sys.argv.(1)
	else Formula.of_string "0&p"
(* Weak vetos are needed in general so default to YES *)
let verbose  = 
	try (Sys.getenv "BATL_VERBOSE" = "Y")
	with Not_found -> false ;;

print_endline "Read formula";;

let colour_limit = 1000000

(* \subsection{Hues} *)
(* We define a Hue as a set of formulas, however a "Hue" is only a
   hue as defined the the B-ATL* paper iff the Hue.valid function
   returns true *) 
module Hue = struct
	include Set.Make(Formula)
	let to_string x = "{" ^ (String.concat ", " (List.map Formula.to_string (elements x))) ^ "}";;
	let println x = print_string ((to_string x) ^ "\n") ;; 
	let bigunion = List.fold_left union empty
	let union3 a b c= union a (union b c)
	
	let rec of_list l = match l with [] -> empty | h::t -> add h (of_list t) 
	
(* === Closure === *)
	let rec closure_of p =
		let r = closure_of in
		let p_notp = of_list [p; neg p] in
		match p with
		| ATOM c      -> p_notp 
		| NOT x       -> add p (r x)
		| NEXT x      -> union p_notp (r x)
		| AND (x,y) | UNTIL (x,y) -> union3 p_notp (r x) (r y)
		| ALLPATH x -> union p_notp (r x)
		| FALSE -> empty 
	
	let closure = closure_of phi;;
	
	print_string (Printf.sprintf "\n Size of closure %d \n" (cardinal closure))

(* === MPC === *)	
	let mpc h = for_all (fun b -> let has x = mem x h in 
					match b with
					| NOT  a    -> ( (has b) != (has a) )
					| AND (x,y) -> ( (has b)  = ((has x) && (has y)) )
					| _ -> true
				) closure;;

(* === Hue === *)
	(* NOTE: the paper currently only has the `vetos\_valid` test on colours. either is correct, but this ways if faster. Maybe change paper?*)
	let valid h = (mpc h) &&  
		for_all (fun p ->
			let has x = mem x h in
			match p with 
			| UNTIL(a,b) -> (has a) || (has b)
			| NOT (UNTIL (a,b)) -> (not(has b))
			| ALLPATH a ->     ( has a )
			| _ -> true
			) h;;


	print_endline "Building Hues";;	
(*i    let all_hues = List.filter valid (List.map of_list (subsets (elements closure)));; i*)
	let all_hues = 
		let out = ref [] in
		 iter_small_subsets
			 (fun  hl->let h = of_list hl in
				if   (valid h)
				then out := h::(!out) 
			) max_int (elements closure);
		(!out);;
		
	print_endline "Built Hues";;	

(* === Hue-rx === *)

	let rx h g = for_all (fun x -> match x with
		| NEXT a       ->      mem a g
		| NOT (NEXT a) -> not (mem a g)
		| UNTIL(a,b)   ->     (mem b h) || (mem x g)
		| NOT (UNTIL(a,b))  ->     (mem a h) ==> (mem x g)
		| _ -> true
	) h

(* === Hue-ra === *)

	(* in ra iff state_atoms and can_formulas the same *)

	let state_atom p =  (*NOTE: in the paper, all atoms are path atoms *)
		match p with
		| ATOM c -> c >= 'a' && c <= 'z'
		| _     -> false
	let state_atoms h = filter state_atom h;;

	let can_formula p = 
		match p with
		| ALLPATH _ -> true
		| NOT ALLPATH _ -> true
		| _     -> false
		
	let can_formulas h = filter can_formula h;;

	assert ((can_formulas (singleton(ATOM 'p')))=empty);
	;;

(* The Hues are now implemented, we now do some Input/Output defintions *)
	
	print_string(Printf.sprintf "\nNumber of Hues: %d \n" (List.length all_hues) );;

(* Since we will have to implement pruning of Colours later, let us
   practice pruning hues that are not even LTL-consistent *)
		
	let has_successor hues h = List.exists (rx h) hues;;
	let filter_hues hues = List.filter (has_successor hues) hues
	let all_hues = fixpoint filter_hues all_hues;; 
	
	print_string(Printf.sprintf "\nNumber of Hues with successors: %d \n" (List.length all_hues) );;
	
	let directly_fulfilled b hues = List.filter (fun h->mem b h) hues;;
	
	let _ = List.iter println (directly_fulfilled (ATOM 'a') all_hues);;
	
	(* returns a list of hues in "hues" that are fulfilled by arleady fulfilled hues in "fh" *)  
	let fulfilled_step hues b fh = List.filter 
		(fun h-> List.exists (fun g-> (equal g h) || (rx h g)) fh)
		hues ;;
	let fulfilled hues b = fixpoint (fulfilled_step hues b) (directly_fulfilled b hues)
	
	let all_fulfilled start_hues =
		let hues = ref (filter_hues start_hues) in
		iter (fun f -> match f with
			 | UNTIL(a,b) -> let ful_b = fulfilled (!hues) b in
							let new_hues = List.filter (fun h->
								mem (UNTIL(a,b)) h ==> List.mem h ful_b
							) (!hues) in
							hues := new_hues
			| _ -> ())
			closure;
		(!hues)
	
	let all_hues = fixpoint all_fulfilled all_hues;;
	
	print_string (Printf.sprintf "Number of LTL-Consistent Hues: %d \n\n" (List.length all_hues));;
	
	let _ = List.iter println all_hues;;
end


let max_hues_in_colour = 
	if Array.length Sys.argv > 2
	then int_of_string Sys.argv.(2)
	else int_of_float (log (float_of_int colour_limit) /. log (float_of_int (List.length Hue.all_hues)));;
(*i let max_hues_in_colour = 2;; i*)
print_string (Printf.sprintf "Limiting ourselves to %d hues per colour\n" max_hues_in_colour);;

flush stdout;

(* \subsection{Colours} *)

module Colour = struct
	include Set.Make(Hue)
	let rec of_list l = match l with [] -> empty | h::t -> add h (of_list t) 
	
	let mem_f f c = 
		exists (fun h->
			Hue.mem f h
		) c;;
		
	assert (mem_f (ATOM 'p') (singleton(Hue.singleton(ATOM 'p'))));;

	let to_string x = "{" ^ (String.concat ", " (List.map Hue.to_string (elements x))) ^ "}";;

(* === Colour === *)
	let valid c =
		let arbitrary_hue = min_elt c in
		let can_f   = Hue.can_formulas arbitrary_hue in
		let state_a = Hue.state_atoms  arbitrary_hue in
		let sat_c1  = for_all (fun h-> 
(*i			let cf_h=Hue.can_formulas h in
			let yn = (cf_h=can_f) in
			printf "i%d  U%d %s \n" (Hue.cardinal  (Hue.inter cf_h can_f)) (Hue.cardinal  (Hue.union cf_h can_f)) (bool2str yn);
			printf "i%s  U%s %s \n" (Hue.to_string (Hue.inter cf_h can_f)) (Hue.to_string (Hue.union cf_h can_f)) (bool2str yn);
			printf "%s ?? %s %s \n" (Hue.to_string cf_h) (Hue.to_string can_f) (bool2str yn); i*)
			Hue.equal (Hue.can_formulas h) can_f &&
			Hue.equal (Hue.state_atoms  h) state_a 
		) c in
		let sat_c2 =
			Hue.for_all (fun f->
				match f with
				| NOT ALLPATH alpha -> mem_f (NOT alpha) c
				| ALLPATH alpha -> true
				| _	-> assert(false)
			) can_f in
		let _ = if verbose then print_endline ((String.concat "" (List.map bool2str [sat_c1;sat_c2])) ^ (to_string c)) in
		(sat_c1 && sat_c2);;

	print_endline "building_all_colours";;

	let all_colours = 
		let out = ref [] in
		iter_small_subsets 
			(fun hl->let c=of_list hl in if ((cardinal c) > 0 && valid c) then out:=c::(!out))
			 max_hues_in_colour
			 Hue.all_hues;
		(!out);;

	let println x = print_string ((to_string x) ^ "\n") ;; 

	if verbose then print_string (String.concat "\n" (List.map to_string all_colours));; 
		printf "\nNumber of Colours: %d" (List.length all_colours);;
	print_newline();;

(* === Colour-rx === *)

(* Note: Ocaml has limits capitization of functions *)

	let rx c d = 
		for_all (fun g -> 
			exists (fun h -> Hue.rx h g) c
		) d

end	

(* \subsection{Pruning Rules} 
   We now define the pruning rules of the tableau. We begin by defining instances.
   *)

(* === Prune === *)

module Instance = struct
  type t = Colour.t * Hue.t
  let compare = compare
  let to_string (c,h) = "Col: " ^ (Colour.to_string c) ^ "Hue: " ^ (Hue.to_string h)
end


module InstanceSet = struct
	include Set.Make(Instance)
	
	let fulfilled_step beta cl (prev: t) = 
		let ret = ref prev in
		List.iter (fun (c: Colour.t) ->
			Colour.iter (fun h ->
				if (Hue.mem beta h) || (
					exists (fun (d,g) ->
						(Colour.rx c d) &&
						(Hue.rx h g) &&
						(Colour.mem g d)
					) (!ret)
				) then ret := add (c,h) (!ret)
			) c
		) cl;
		(!ret)
        
	let fulfilled (beta: Formula.t) cl = fixpoint (fulfilled_step beta cl) empty

	let println= iter (fun inst -> print_string ((Instance.to_string inst) ^ "\n") )
end

let satisfied_by colours = List.exists (fun c ->
		let has_phi = Colour.mem_f phi c in
		if has_phi then (
			printf "\nphi in %s \n" (Colour.to_string c);
			if verbose 
			then List.iter (fun d -> if Colour.rx c d then printf " RX -> %s " (Colour.to_string d)) colours
		);
		has_phi
	) colours;;


let log_prune n ch col = (
	let _ = satisfied_by col in
	print_string (Printf.sprintf "Before rule %d%c:  Number of Colours: %d" n ch (List.length col));
)

(* === Prune1 === *)
let prune_rule_1 colours =
		log_prune 1 ' ' colours;
		print_newline();
		List.filter (fun (c: Colour.t) ->
			not (
				Colour.exists (fun h ->
					List.for_all (fun d->
						(not (Colour.rx c d)) ||
						Colour.for_all (fun g-> not (Hue.rx h g)) d
					) colours
				) c 
			)
		) colours;;

(* === Prune2 === *)
let prune_rule_2 in_colours =
		log_prune 2 ' ' in_colours;
		print_newline();
		let colours = ref in_colours in
		Hue.iter (fun f -> match f with
			| UNTIL(a,beta) -> 
				let ful_b = InstanceSet.fulfilled beta (!colours) in
								if verbose then InstanceSet.println ful_b;
				let new_colours = List.filter (fun c->
					Colour.for_all ( 
						fun h-> (Hue.mem f h) ==>  InstanceSet.mem (c,h) ful_b (*NOTE: check "non-vetoed" in paper*)
					) c
				) (!colours) in
				colours := new_colours 
			| _ -> ())
			Hue.closure;
		(!colours);;

let prune_step colours = (prune_rule_2 (prune_rule_1 colours))

let prune = fixpoint prune_step;;

let remaining_colours = prune Colour.all_colours;;

print_string (Printf.sprintf "Number of colours remaining %d\n" (List.length remaining_colours));;
if verbose then print_string (String.concat "\n" (List.map Colour.to_string remaining_colours));; 

(* \subsection{Result} 
	We now return the result as to whether the input formula was satisfiable or not.
	If we have excluded large colours then determine that the formula was
	unsatisifable, however if we may have found a model in the restricted tableau
	in which case it clearly is satisfiable *)

let result = Printf.sprintf "Finished Processing %s\n" (Formula.to_string phi) ^
if satisfied_by remaining_colours
then "RESULT: SATISFIABLE"
else 
	if (max_hues_in_colour < List.length Hue.all_hues) 
	then Printf.sprintf "Not satisfied, but large colours with more than %d (of %d) hues have been excluded\nRESULT: UNKNOWN" max_hues_in_colour (List.length Hue.all_hues) 
	else  
		"RESULT: UNsatisfiable"

let result = let num_hues = (List.length Hue.all_hues) in
	result ^ Printf.sprintf " #Hues=%s%d #Colours=%d #Remaining=%d"
		(if (max_hues_in_colour<num_hues) then (string_of_int max_hues_in_colour) ^"/" else "")
	num_hues
	(List.length Colour.all_colours)
		(List.length remaining_colours);;

print_endline result;;
