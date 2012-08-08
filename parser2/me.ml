#!/usr/bin/env ocaml

module StringSet=Set.Make(String)
module StringMap=Set.Make(String)

let append = fun a x -> let (l,e) = a in  e.(l) <- x; (l+1,e) 

(* Now we start our mathematical definitions *)

type label = LabelC of char | LabelS of StringSet.t;;
(*type tree = { l : label; c : tree list; };;*)
type 'a tree = {l: 'a; c: 'a tree list}
(*type 'a node = { ee: 'a; ec: int array }
type 'a node = { ee: 'a; ec: int array }
type 'a array2 = { ae: 'a array; al: int }
otype 'a dag = 'a node array2;
*)

type 'a array2 = {a: 'a array; mutable len: int}
type 'a dag = ('a * int array) array2

type 'a dagElem = int * 'a dag

let append = fun x aa ->
	 let l=aa.len in 
	 aa.a.(l) <- x;
	 aa.len <- l+1;
	 l

(*let adds = fun aam x -> let (aa,m)=aam in key if StringMap.mem s then StringMap.find   
*)

let rec dag_from_tree_ d t = 
	let children = List.map (dag_from_tree_ d) t.c in
	append (Array.of_list children) d

type melement = { op : label; mutable pIMH: int; mutable pMHI: int }
type formula_op = char
type formula_dag = formula_op dag
type formula = formula_op dagElem
type me_dag = melement dag
type me_context = {med : me_dag; fd : formula_dag}
type me = {m : int ; mc : me_context }

(*type me = int * me_context *)

let tttt = {op=o} 
let new_melement = fun o ->  {op=o; pIHM= (-1) } 
let new_melement = fun o -> {op=o; pIMH=(-1); pMHI=(-1)} 

let (<:) = fun e -> if mc.med.a(m) < 0 then e with m = append (new_melement '<', [|e.m|]) me.med     

 

(*let rec string_dag_from_tree_ d t = 
	let children = List.map (dag_from_tree_ d) t.c in
	append d (Array.of_list children)
*)






type me = int * melement


let ttt={l='a';c=[]}



type fcell = { fcOp : char; fcCh : int array; }
type formula = { fId : int; fSpace : int * fcell array }  

(*let fU a b = *)

type melement = { eOp : char; eCh : int array; eIMH : int ; eHMI : int  }
type me = { meId : int; meSpace : melement array }

let new_melement = fun o c -> {eOp=o; eCh=c; eIMH = -1 ; eHMI = -1}


