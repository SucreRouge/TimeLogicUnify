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


module Flipper = struct
	let empty=IntSet.empty
	let flip i s = if IntSet.mem i s then IntSet.add i s else IntSet.remove i s
	let join s t = IntSet.union s t
end

module IntSet2Int = Map.Make(IntSet,Int);;

module FlipperW = struct
	let memo = HashTbl.create 9;
	let flipper2int_map=ref IntSet2Int.empty;
	let int2flipper_map=ref Int2IntSet.empty;
	let empty = 0; 
        let _ = HashTbl.add memo 0 IntSet.empty;

	let flip i s = if not Hashtbl.mem (flip,i,s) then Hashtbl.add (flip,i,s) (Flipper.flip  else  
