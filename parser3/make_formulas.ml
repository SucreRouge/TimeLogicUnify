#!/bin/env ocaml
(* Added from CygWin. Depreciated? *) 

let printf = Printf.printf
let s = string_of_int
let sm x = (s (x-1))
let sp x = (s (x+1))
let p i = "p" ^ (s i)
let neg x = "~" ^ x
let (+:) x y = "(" ^ x ^ "+" ^ y ^ ")"
let u x y  = "U(" ^ x ^ ", " ^ y ^ ")"
let (~<) x  = "<" ^ x 
let rec me i = (if i<=0 then "{p1}" else 
        ~<((p i) +: (me (i-1))))
        (*"<(p"^s(i)^"+"^(me (i-1))^")" *)
let alpha i = u (p i)  (neg(p (i+1)))

let rec formula i = (if i<=1 then alpha 1 else alpha i ^ "|" ^ (formula (i-1) ))

;;

let ii = (int_of_string Sys.argv.(1)) in
printf "%s : %s\n" (formula ii) (me ii)
