#!/usr/bin/env ocaml

(* lead = <-
   trail = -> *)


module StringSet=Set.Make(String)
module StringMap=Set.Make(String)

let append = fun a x -> let (l,e) = a in  e.(l) <- x; (l+1,e) 

(* Now we start our mathematical definitions *)

type label_t = LabelC of char | LabelS of bool array;;
(*type tree = { l : label; c : tree list; };;*)
type 'a tree = {l: 'a; c: 'a tree list}
(*type 'a node = { ee: 'a; ec: int array }
type 'a node = { ee: 'a; ec: int array }
type 'a array2 = { ae: 'a array; al: int }
otype 'a dag = 'a node array2;
*)

type 'a array2 = {mutable a: 'a array; mutable len: int}
type 'a dag_node = {n: 'a ; d:  int array}
type 'a dag =  'a dag_node array2


let new_dag = fun () -> {a=[| |]; len=0} 

let new_dag_from = fun d -> if d.len = 0
	then new_dag()
	else {a=Array.make (Array.length d.a) d.a.(0); len=0}
		

type 'a dagElem = int * 'a dag

let append = fun x aa ->
	 let l=aa.len in 
		if l >= Array.length aa.a then
			if l = 0 then 
				aa.a <- [|x|]
			else 
				aa.a <- Array.append aa.a aa.a;
	 aa.a.(l) <- x;
	 aa.len <- l+1;
	 l

(*let adds = fun aam x -> let (aa,m)=aam in key if StringMap.mem s then StringMap.find   
*)

let aa_to_array aa = Array.sub aa.a 0 aa.len 

let arrayi_to_string = fun j f a -> String.concat j (Array.to_list (Array.mapi f a)) 
let array_to_string  = fun j f a -> String.concat j (Array.to_list (Array.map  f a)) 
let intarray_to_string = array_to_string ", " string_of_int
let rec dag_from_tree_ d t = 
	let children = List.map (dag_from_tree_ d) t.c in
	append {n=t.l; d=(Array.of_list children)} d

let dag_from_tree = fun t -> let d = {a=[| |]; len=0} in 
	let _ = dag_from_tree_ d t in
	d

(* slow refactor *)
(*
let dag_to_string = fun f d -> Array.mapi (fun i dn -> ((int_to_string i) ^ " " ^ (f dn) ^ " " ^ (intarray_to_string dn.d) ^ "\n" )) (aa_to_array d)*)
let print_dag = fun f d -> Array.mapi (fun i dn -> Printf.printf "%d %s %s\n" i (f dn.n) (intarray_to_string dn.d) ) (aa_to_array d)

type melement = { op : label_t; mutable pIHM: int; mutable pMHI: int }
type formula_op = char 
type formula_dag = formula_op dag
type formula = formula_op dagElem
type me_dag = melement dag
type me_context = {med : me_dag; fd : formula_dag}
type model_expression = {m : int ; md : me_dag; fd : formula_dag }

let new_melement = fun o -> {op=LabelC o; pIHM=(-1); pMHI=(-1)} 

let lead = fun e -> let e_node = e.md.a.(e.m).n in
  if e_node.pMHI < 0 then e_node.pMHI <- append {n=new_melement '<'; d=[|e.m|]} e.md;
  {e with m=e_node.pMHI}

let trail = fun e -> let e_node = e.md.a.(e.m).n in
  if e_node.pIHM < 0 then e_node.pIHM <- append {n=new_melement '>'; d=[|e.m|]} e.md;
  {e with m=e_node.pIHM}

let shuffle_ = fun md fd a -> {m=(append {n=new_melement 'S'; d=a} md); md=md; fd=fd}
let letter_ = fun md fd atoms -> {m=append {n={op=LabelS atoms; pIHM=(-1); pMHI=(-1)}; d=[||]} md; md=md; fd=fd} 
let (~<) = lead 
let (~>) = trail 
let (=>) = fun x y -> (not x) || y
let (<=) = fun x y -> x || (not y)

let (+~) = fun e g -> if ( e.md == g.md && e.fd == g.fd ) 
  then {e with m=append {n=new_melement '+'; d=[|e.m;g.m|]} e.md}
  else failwith "ME_Context_Mismatch"

let (+:) = (+~)

type cacheU = { pre_false: int ; pre_true: int; all_p: bool; some_q: bool }  

(*
let build_cacheU d fd f = let ca = Array.make d.len {pre_false=false; pre_true=false; all_q = false; some_p = false} in
	let pre_i = fun i b -> if b then ca.(i).pre_true else ca.(i).pre_false in
	let allp_i = fun i -> ca.(i).all_p in 
	let (p,q) = (fd.d.(0),fd.d(1)) in
	for i = 0 to d.len - 1
	do 
		let dai = d.a.(i) in
		ca.(i) <- match dai.n with 
			LabelC label -> 
				let child = dai.d in 
				let all_q  = Array.fold_left (fun x a -> x && ca.(i).all_q ) true  child in
				let some_p = Array.fold_left (fun x a -> x || ca.(i).some_p) false child in
				let pre = fun b -> match label with 
					'+'   ->  pre_i ( child.(0), pre_i ( child.(1), b))
					| '>' ->  pre_i ( child.(0), b )
					| _   ->  (b || some_p) && all_q in
				{ pre_false = pre false; pre_true = pre true; all_q = all_q; some_p = some_p}
			| LabelS atoms ->
				let pre = fun b -> atoms.(p) || ( b  && atoms.(q) ) in
				{ pre_false = pre false; pre_true = pre true; all_q = atoms.(q); some_p = atoms.(p)}
	done		
*)

let bool_to_int = fun b -> if b then 1 else 0

let swap_pair = fun a -> 
	let a0 = a.(0) in
	let a1 = a.(1) in 
        a.(0) <- a1; a.(1) <- a0

let mirror d = for k = 0 to d.len - 1 do 
		let dak = d.a.(k) in
		d.a.(k) <- match dak.n.op with 
			LabelC label -> (
                                match label with  
                                        '+'   ->  swap_pair dak.d; dak
                                        (*ALT: '+'   ->  {dak with d=[|mI.(1); mI.(0)|]}*)
                                        | '<' ->  {dak with n={dak.n with op=LabelC '>'}}
                                        | '>' ->  {dak with n={dak.n with op=LabelC '<'}}
                                        | 'S' ->  dak
					| _ -> failwith "Invalid_ME_Operator"
				
			)
                        | LabelS atoms -> dak
	done


let build_pre = fun d fd f ->
	let pre = (Array.make_matrix (d.len) 2 (-1)) in
	let all_q = Array.make d.len false in
	let some_p = Array.make d.len false in
	let pq = (fd.a.(f).d) in
	let (p,q) = (pq.(0),pq.(1)) in
	for k = 0 to d.len - 1
	do 
		let dak = d.a.(k) in
		match dak.n.op with 
			LabelC label -> 
				let children = dak.d in 
				all_q.(k)  <- Array.fold_left (fun x i -> x && all_q.(i) ) true  children;
				some_p.(k) <- Array.fold_left (fun x i -> x || some_p.(i)) false children;
				let mI = children in 
				let pre_ = fun b -> match label with 
					'+'   ->  pre.(mI.(0)).(pre.(mI.(1)).(b))
					| '>' ->  pre.(mI.(0)).(b)
					| _   ->  bool_to_int (((b==1) || some_p.(k)) && all_q.(k)) in
				( pre.(k).(0) <- pre_ 0;
				  pre.(k).(1) <- pre_ 1 )
			| LabelS atoms ->
				let pre_ = fun b -> bool_to_int (atoms.(p) || ( (b==1)  && atoms.(q) )) in
				( pre.(k).(0) <- pre_ 0;
				  pre.(k).(1) <- pre_ 1;
				  all_q.(k) <- atoms.(q);
				  some_p.(k) <- atoms.(p) )
	done ;
	pre

let safe_set a i x = let c = (Array.copy a) in Array.set c i x ; c

(* Should merge for performance *)
let add_atom_Upq = fun  d_in fd f ->
	let d_out = new_dag_from d_in in
	let shuffle = (shuffle_ d_out fd) in
	let letter = (letter_ d_out fd) in
	let pre = build_pre d_in fd f in
	let _ = pre.(0).(0) + 1 in 
	let t_cache = Array.make_matrix d_in.len 2 (-1) in
	let rec t_rec k b =
		let t_ = fun i b -> (
			let _ = if t_cache.(i).(b) < 0 then t_cache.(i).(b) <- t_rec k b else () in
			t_cache.(i).(b)
		) in
		let t = fun i b -> {m=t_ i b; md=d_out; fd=fd} in 
		let dak = d_in.a.(k) in
		( match dak.n.op with
			LabelC label -> (
                                let mI = dak.d in
				let mI0 = mI.(0) in 
                                ( match label with  
                                        '+'   ->  ( (t mI0 (pre.(mI.(1)).(b))) +: (t mI.(1) b) ) 
                                        | '<' ->  ( ( ~< ( t mI0 pre.(mI0).(b)) ) +: (t mI0 b) )
                                        | '>' ->  ( ~> ( t mI0 pre.(mI0).(b)) )
                                        | 'S' ->  (shuffle (Array.map (fun i -> t_ i b) mI))
					| _ -> failwith "Invalid_ME_Operator"
				)
			)
                        | LabelS atoms -> 
				let atoms = if (b==1) then safe_set atoms f true else atoms in
				letter atoms 
		).m
	in
	ignore (t_rec (d_in.len-1) 0) ;
	d_out 

let add_atom_Spq = fun  d_in fd f ->
	let _ = mirror d_in in 
	let d_out = add_atom_Upq d_in fd f in
	let _ = mirror d_out in
	d_out

let add_atom_PC = fun d fd f bool_func ->
	for k = 0 to d.len - 1
	do 
		let dak = d.a.(k) in
		match dak.n.op with 
			LabelC label -> ()
			| LabelS atoms -> if bool_func (atoms)
				then Array.set atoms f true
				else ()
	done ; d

let add_atom = fun d fd f ->
	let pq = (fd.a.(f).d) in
	let f_op = fd.a.(f).n in 
	let pc = (add_atom_PC d fd f) in
	match (Array.length pq) with
		0 -> d
		| 1 -> assert (f_op = '-') ; let p = pq.(0) in 
                        pc (fun atoms -> not atoms.(p))
		| 2 -> let (p,q) = (pq.(0),pq.(1)) in ( match f_op with
			  '&' -> pc (fun a -> a.(p) && a.(q)) 
			| '|' -> pc (fun a -> a.(p) || a.(q))
			| '=' -> pc (fun a -> a.(p) =  a.(q))
			| '>' -> pc (fun a -> a.(p) => a.(q))
			| '<' -> pc (fun a -> a.(p) <= a.(q))
			| 'U' -> add_atom_Upq d fd f	
			| 'S' -> add_atom_Spq d fd f
			| _   -> failwith "Invalid_Operator" )
		| _ -> failwith "More_than_two_children_in_formula"

let add_atoms = fun d fd ->
	let d_out = ref d in 
	for f = 0 to fd.len -1
	do
		d_out := add_atom (!d_out) fd f
	done ; d_out
		
let ttttt=2	
		 
let _ = print_dag (fun xx -> xx) ( dag_from_tree {l="a"; c=[{l="b";c=[]}]} );


(*	
let t_cache f a_true a_false i b = 
	IF 
	if a.(i) < 0 then
		a.(i) <- f i;
	a.(i) 
		 
let rec tau_ d_out d_in t_true t_false fd p q it_true t_false 
let add_atom_Upq d p q  
let add_atom_Upq_ = fun d_out d_in t_true t_false fd p q i ->
	let pre = fun i b -> if b then ca.(i).pre_true else ca.(i).pre_false in
	let pre i b = 
	let rec t_rec k b =
		let t = fun i b -> let t_cache = if b then t_true else t_false in 
			if t_cache.(i) < 0 then 
				t_cache.(i) <- t_rec k b;
			t_cache.(i)
		let dai = d.a.(k) in
		match dai.n with
			LabelC label ->
                                let child = dai.d in 
                                match label with 
					let i = child.(0) in 
                                        '+'   ->  let j = child.(1) in t(i, +: j) 
                                        | '>' ->  pre_i ( child.(0), b )
                                        | _   ->  (b || some_p) && all_q in
                                { pre_false = pre false; pre_true = pre true; all_q = all_q; some_p = some_p}
                        | LabelS atoms ->
                                let pre = fun b -> atoms.(p) || ( b  && atoms.(q) ) in
                                { pre_false = pre false; pre_true = pre true; all_q = atoms.(q); some_p = atoms.(p)}
		t 
	
*)
  



(*type me = int * me_context *)

(*type me = int * me_context *)
(*

*)
(*


let (!<) = fun e -> let e_node = e.mc.med.a.(e.m).n in
  if e_node.pMHI < 0 then e_node.pMHI <- append {n=new_melement '<'; d=[|e.m|]} e.mc.med;
  {m=e_node.pMHI; mc = e.mc};
*)
 


(*let rec string_dag_from_tree_ d t = 
	let children = List.map (dag_from_tree_ d) t.c in
	append d (Array.of_list children)
*)




(*

type me = int * melement


let ttt={l='a';c=[]}



type fcell = { fcOp : char; fcCh : int array; }
type formula = { fId : int; fSpace : int * fcell array }  

(*let fU a b = *)

type melement = { eOp : char; eCh : int array; eIMH : int ; eHMI : int  }
type me = { meId : int; meSpace : melement array }

let new_melement = fun o c -> {eOp=o; eCh=c; eIMH = -1 ; eHMI = -1}

ddlet _ = print_dag (fun xx -> xx) ( dag_from_tree {l="a"; c=[{l="b";c=[]}]} );
*)
