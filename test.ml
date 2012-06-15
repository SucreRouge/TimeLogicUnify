type tree = { l : char; c : tree list; }
 type label = LabelC of char | LabelS of float;;
let x = {l='x'; c=[{l='z';c=[]}]};;

let f x = x+1;;

 List.map f [1; 2;8];;


StringSet.mem "b" a 
let a = StringSet.add "a" StringSet.empty;;
 type label = LabelC of char | LabelS of StringSet;;
let x = {l='x'; c=[{l='z';c=[]}]};;


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


let ttt = {l='x'; c=[{l=letter "pq";c=[]}]};;


StringSet.elements emptyLetter;;



let rec tree_and t p q = match t.l with
     LabelC l -> {l; List.map (function tt -> tree_and tt p l) t.c }
     StringSet s -> s.add p;;   

let rec tree_iter t f = {l=f(t.l); c=List.map (function tt -> tree_and tt f) t.c};;

tree_and = fun  
