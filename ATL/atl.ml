module Int = struct
  type t = int
  let compare = compare
end

module IntSet = struct
	include Set.Make(Int)
	let rec of_list l = match l with [] -> empty | h::t -> add h (of_list t) 
end

module ISS = Set.Make(IntSet)

let agents_disjoint al = 
	let rec r prev l =
		match l with
		| [] -> true
		| head::tail -> 
			if (IntSet.inter prev head = IntSet.empty)
			then r (IntSet.union prev head) tail 
			else false in
	r IntSet.empty (ISS.elements al)

let println_list_of_int li = Printf.printf "[%s]\n" (String.concat "; " (List.map string_of_int li))

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



let rec range i j = if i > j then [] else i :: (range (i+1) j)

let ( ==> ) a b = ((not a) || b)

type coalition = IntSet.t

let coalition_to_string c = "{" ^ (String.concat "" (List.map string_of_int (IntSet.elements c))) ^"}"

let rec fixpoint f x = let fx = f x in
	if (x = fx) 
	then x
	else fixpoint f fx;;
	
assert ( (fixpoint (fun x -> x/2) 9) = 0);;

(* We extend the (B)ATL* formulas with "STRONG" and "WEAK" vetos *)

type formula =
    | ATOM of char
    | NOT of formula
    | AND of formula * formula   
    | NEXT of formula
	| UNTIL of formula * formula
    | CAN of coalition * formula (* a Strategy: Can A psi = <<A>>psi *)
    | STRONG of coalition (* Strong Veto *)
    | WEAK of coalition (* Weak Veto *)


module Formula = struct
	type t = formula 
	let rec to_string psi = let s = to_string in
		match psi with
	    | ATOM c      -> (String.make 1 c)
		| NOT x       -> "~" ^ (s x)
		| NEXT x      -> "X" ^ (s x)
		| AND (x,y)   -> "(" ^ (s x) ^ "&" ^ (s y) ^ ")"
		| UNTIL (x,y) -> "(" ^ (s x) ^ "U" ^ (s y) ^ ")"		
		| CAN (a,y)   -> (coalition_to_string a) ^ (s y)
		| STRONG a -> "V" ^ (coalition_to_string a)
		| WEAK  a -> "v" ^ (coalition_to_string a)
		
	let print x = (print_string (to_string x))
	let println x = print_string ((to_string x) ^ "\n") 
	let compare = compare
end  

	

(* We now fix a formula that we wish to decide *)
(*
let phi = AND (CAN (IntSet.singleton(1), NOT (NEXT (ATOM 'p'))), CAN (IntSet.empty, ATOM 'p')) 
let phi = NEXT (AND (ATOM 'p', NOT (ATOM 'p'))) 
*)


let phi = UNTIL (ATOM 'p', (AND (ATOM 'p', NOT (ATOM 'p'))))

let num_agents = 1

let all_agents = IntSet.of_list(range 1 num_agents)

(* We define a Hue as a set of formulas, however a "Hue" is only a
   hue as defined the the B-ATL* paper iff the Hue.valid function
   returns true *) 
module Hue = struct
	include Set.Make(Formula)
	let bigunion = List.fold_left union empty
	let union3 a b c= union a (union b c)
	
	let rec of_list l = match l with [] -> empty | h::t -> add h (of_list t) 
	
	let rec closure_of p =
		let r = closure_of in
		let p_notp = of_list [p; NOT p] in
		match p with
	    | ATOM c      -> p_notp 
		| NOT x       -> add p (r x)
		| NEXT x      -> union p_notp (r x)
		| AND (x,y) | UNTIL (x,y) -> union3 p_notp (r x) (r y)
		| CAN (a,y)   -> union3 p_notp (of_list [STRONG a; WEAK a]) (r y)  
		| STRONG a | WEAK a -> singleton p

		(* FIXME: should there ever be "WEAK empty" in the closure *)
		
	let closure = closure_of phi
	
	let mpc h = for_all (fun b -> let has x = mem x h in 
					match b with
					| NOT a ->   ( (has b) != (has a) )
					| AND (x,y) -> ( (has b)  = ((has x) && (has y)) )
					| _ -> true
				) closure;;

	let rec add_vetos prev_vetos h =
		let rec r (w,s) h =
			match h with
			| [] -> prev_vetos
			| (WEAK   ag)::tail -> r ((ISS.add ag w), s) tail
			| (STRONG ag)::tail -> r (w, (ISS.add ag s)) tail
			| _::tail -> r (w,s) tail in
		r prev_vetos (elements h)
	let get_vetos = add_vetos (ISS.empty, ISS.empty)
	let vetos_valid (w,s) = ((ISS.cardinal w) <= 1) && (agents_disjoint (ISS.union w s));;

	let valid h = (mpc h) && (vetos_valid (get_vetos h)) &&  
				  for_all (fun p ->
				    let has x = mem x h in
					match p with 
					| UNTIL(a,b) -> (has a) || (has b)
					| NOT (UNTIL (a,b)) -> (not(has b))
					| CAN(x,a) ->      ( (x=IntSet.empty) ==> (has a) )
					| NOT CAN(x,a) ->  ( (x=all_agents) ==> (not (has a)))
					| _ -> true
					) h;;

	
    let all_hues = List.filter valid (List.map of_list (subsets (elements closure)));;

    let rx h g = for_all (fun x -> match x with
		| NEXT a       ->      mem a g
		| NOT (NEXT a) -> not (mem a g)
		| UNTIL(a,b)   ->     (mem b h) || (mem x g)
		| WEAK(s) | STRONG (s) -> (mem x g)
		| _ -> true
	) h

	let can_formula p = 
		match p with
		| CAN _ -> true
		| _     -> false
		
	let can_formulas h = filter can_formula h;;       

	assert ((can_formulas (singleton(ATOM 'p')))=empty);
	assert ((can_formulas (singleton(CAN (IntSet.empty, ATOM 'p'))))!=empty);
	;;
 
	let ra h g = 
		let r h g = for_all (fun x -> 
			match x with
			| UNTIL(a,b)   ->     (mem x g)
			| _ -> true) h in
		(r h g) &&  (r g h);;

	let vetoed = 
		exists (fun psi->
			match psi with
			| STRONG _ | WEAK _ -> true;
			| _ -> false;
		) 
	let not_vetoed h = not (vetoed h)			

(* The Hues are now implemented, we now do some Input/Output defintions *)
					
    let to_string x = "{" ^ (String.concat ", " (List.map Formula.to_string (elements x))) ^ "}";;
    let println x = print_string ((to_string x) ^ "\n") ;; 

	let _ = iter Formula.println closure;; 
    
    let _ = print_string (String.concat "\n" (List.map to_string all_hues));;
    
    Printf.printf "\nNumber of Hues: %d \n" (List.length all_hues);;

(* Since we will have to implement pruning of Colours later, let us
   practice pruning hues that are not even LTL-consistent *)
 		
	let has_successor hues h = List.exists (rx h) hues;;
	let filter_hues hues = List.filter (has_successor hues) hues
	let all_hues = fixpoint filter_hues all_hues;; 
	
    Printf.printf "\nNumber of Hues with successors: %d \n" (List.length all_hues);;
	
	let directly_fulfilled b hues = List.filter (fun h->mem b h) hues;;
	
    let _ = List.iter println  all_hues;;

    let _ = List.iter println (directly_fulfilled (ATOM 'a') all_hues);;
 	
	(* returns a list of hues in "hues" that are fulfilled by arleady fulfilled hues in "fh" *)  
	let fulfilled_step hues b fh = List.filter 
		(fun h-> List.exists (fun g-> (g=h) || (rx h g)) fh)
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
		
    Printf.printf "Number of LTL-Consistent Hues: %d \n\n" (List.length all_hues);;
    
	let _ = List.iter println all_hues;;
end
;;
iter_small_subsets println_list_of_int 4 [1; 2; 3];
;;

module Colour = struct
	include Set.Make(Hue)
	
	(* exists a formula satisfying fn
	exists_f fn c = 
		exists (fun h->
			exists fn h
		) c
	*)
	
	let mem_f f c = 
        exists (fun h->
			Hue.mem f h
		) c;;	
		
	let _ =assert (mem_f (ATOM 'p') (singleton(Hue.singleton(ATOM 'p'))));;

	let valid c = 
		let arbitrary_hue = min_elt c in
		let can_f = Hue.can_formulas arbitrary_hue in
		let sat_c1 = for_all (fun h->(Hue.can_formulas h)=can_f) c in 
		let sat_c2 =
			Hue.for_all (fun f->
				match f with
				| CAN(_,alpha) -> mem_f alpha c
				| _	-> assert(false)
			) can_f in
		let sat_c3 = exists Hue.not_vetoed in
		
		(sat_c1 && sat_c2);;
 
(*			
		let sat_c2 =
			for_all (fun f->
				match f with
				| CAN(_,alpha) -> mem_f alpha c
				| _	-> assert(false)
			) can_f in
		(sat_c1 && sat_c2)
		*)
	
end	
