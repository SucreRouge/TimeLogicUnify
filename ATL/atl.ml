module Int = struct
  type t = int
  let compare = compare
end

module IntSet = struct
	include Set.Make(Int)
	let rec of_list l = match l with [] -> empty | h::t -> add h (of_list t) 
end

let subsets xs = List.fold_right (fun x rest -> rest @ List.map (fun ys -> x::ys) rest) xs [[]]
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
		
	let closure = closure_of phi
	
	let mpc h = for_all (fun b -> let has x = mem x h in 
					match b with
					| NOT a ->   ( (has b) != (has a) )
					| AND (x,y) -> ( (has b)  = ((has x) && (has y)) )
					| _ -> true
				) closure;;

	let valid h = (mpc h) && 
				  for_all (fun p ->
				    let has x = mem x h in
					match p with 
					| UNTIL(a,b) -> (has a) || (has b)
					| NOT (UNTIL (a,b)) -> (not(has b))
					| CAN(x,a) ->      ( (x=IntSet.empty) ==> (has a) )
					| NOT CAN(x,a) ->  ( (x=all_agents) ==> (not (has a)))
					| _ -> true
					) h;;
					
    let to_string x = "{" ^ (String.concat ", " (List.map Formula.to_string (elements x))) ^ "}";;
    let println x = print_string ((to_string x) ^ "\n") ;; 

	let _ = iter Formula.println closure;; 
	
    let all_hues = List.filter valid (List.map of_list (subsets (elements closure)));;
    
    let _ = print_string (String.concat "\n" (List.map to_string all_hues));;
    
    Printf.printf "\nNumber of Hues: %d \n" (List.length all_hues);;
    
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
		
	let can_formulas h = filter h;;       
 
   let ra h g = 
		let r h g = for_all (fun x -> 
			match x with
			| UNTIL(a,b)   ->     (mem x g)
			| _ -> true) h in
		(r h g) &&  (r g h);;
		
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



module Colour = struct
	include Set.Make(Hue)
end	
