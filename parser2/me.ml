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

let (~<) = lead 

let trail = fun e -> let e_node = e.md.a.(e.m).n in
  if e_node.pIHM < 0 then e_node.pIHM <- append {n=new_melement '>'; d=[|e.m|]} e.md;
  {e with m=e_node.pIHM}

let (+:) = fun e g -> if ( e.md == g.md && e.fd == g.fd ) 
  then append {n=new_melement '+'; d=[|e.m;g.m|]} e.md
  else failwith "ME_Context_Mismatch"

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

let build_pre = fun d fd ->
	let pre = (Array.make_matrix (d.len) 2 (-1)) in
	let all_q = Array.make d.len false in
	let some_p = Array.make d.len false in
	let (p,q) = (fd.d.(0),fd.d.(1)) in
	for k = 0 to d.len - 1
	do 
		let dak = d.a.(k) in
		match dak.n with 
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
	done

let ttt=2
(*	
let add_atom_Upq_ = fun d_out d_in t_true t_false fd p q i ->
	let pre = build_pre d_in fd in
	let rec t_rec k b =
		let t = fun i b -> let t_cache = if b then t_true else t_false in 
			if t_cache.(i) < 0 then 
				t_cache.(i) <- t_rec k b;
			t_cache.(i)
		let dak = d.a.(k) in
		match dak.n with
			LabelC label ->
                                let mI = dak.d in 
                                match label with 
                                        '+'   ->  t(mI.(0), pre(mI.(1), b) +: t(mI.(1),b) 
                                        | '<' ->  pre_i ( child.(0), b )
                                        | '>' ->  pre_i ( child.(0), b )
                                        | _   ->  (b || some_p) && all_q in
                                { pre_false = pre false; pre_true = pre true; all_q = all_q; some_p = some_p}
                        | LabelS atoms ->
                                let pre = fun b -> atoms.(p) || ( b  && atoms.(q) ) in
                                { pre_false = pre false; pre_true = pre true; all_q = atoms.(q); some_p = atoms.(p)}
		t 
*)

	
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
