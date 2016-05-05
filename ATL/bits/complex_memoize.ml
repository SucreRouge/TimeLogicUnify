module Droid = struct
	type t = string
	let  empty = ""
	let  beep x = x ^ " notdroids"
	let  boop x = x ^ " lookingfor"
	let  len x = String.length x
end

let memo f =
  let m = Hashtbl.create 9 in
    fun x ->
      try Hashtbl.find m x
      with Not_found ->
        let y = f x in
          Hashtbl.add m x y; y

module DroidMemoized = struct
	(* Boilerplate *)
	let to_id_h   : (Droid.t, int) Hashtbl.t = Hashtbl.create 9
	let from_id_h : (int, Droid.t) Hashtbl.t = Hashtbl.create 9 (*or array*)
	let size = ref 0
	let from_id i = (Hashtbl.find from_id_h i)
	let to_id (x :Droid.t) : int = 
		if	Hashtbl.mem  to_id_h x 
		then	Hashtbl.find to_id_h x
		else	(size := (!size)+1;
			Hashtbl.add  to_id_h   x (!size);
			Hashtbl.add  from_id_h (!size) x;
			!size)
	(* Function wrappers *)
	let empty = to_id Droid.empty
	let beep = memo ( fun x -> to_id (Droid.beep (from_id x)) )
	let boop = memo ( fun x -> to_id (Droid.boop (from_id x)) )
	let len  = memo ( fun x ->       (Droid.len  (from_id x)) )
end

(*

let  cat x y = print_string "catting\n" ; x ^ y
let testbeep = (DroidMemoized.beep (DroidMemoized.beep DroidMemoized.empty))
let testboop = (DroidMemoized.boop DroidMemoized.empty)
let _ = print_string ((DroidMemoized.from_id (DroidMemoized.cat testbeep testboop)) ^ "\n")
let _ = print_string ((DroidMemoized.from_id (DroidMemoized.cat testbeep testboop)) ^ "\n")
let _ = print_string ((DroidMemoized.from_id (DroidMemoized.cat testbeep testboop)) ^ "\n")
let _ = print_string ((DroidMemoized.from_id (DroidMemoized.cat testbeep testboop)) ^ "\n")


let cat_ x y = to_id (Droid.cat (from_id x) (from_id y))
	let cat  = memo2 cat_

let memo2 f =
  let m = Hashtbl.create 9 in
    fun x x2 ->
      try
        Hashtbl.find m (x, x2)
      with
      Not_found ->
        let y = f x x2 in
          Hashtbl.add m (x, x2) y;
          y

let memo_old f =
  let m = ref [] in
    fun x ->
      try
        List.assoc x !m
      with
      Not_found ->
        let y = f x in
          m := (x, y) :: !m ;
          y





let double x = print_string "working\n"; x*2
let md = memo double

let test = md 2
let test2 = md 2





	let cat  = memo2 cat
let memo2 f =
  let m = Hashtbl.create 9 in
    fun x x2 ->
      try
        Hashtbl.find m (x, x2)
      with
      Not_found ->
        let y = f x x2 in
          Hashtbl.add m (x, x2);
          y


			Hashtbl.add  to_id_h   (!size) x;
			Hashtbl.add  from_id_h x (!size);

		if	Hashtbl.mem  to_id_h x 
		then	Hashtbl.find to_id_h x
		else	1)
		else	(size := (!size)+1;
			Hashtbl.add  to_id_h   size x;
			Hashtbl.add  from_id_h x size)

*)
