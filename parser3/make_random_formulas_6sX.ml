#!/bin/env ocaml

(* See: http://ocamlnews.blogspot.com.au/2007/08/power-set.html *)
let rec powerset = function
        | [] -> [[]]
        | h::t -> List.fold_left (fun xs t -> (h::t)::t::xs) [] (powerset t);;


let printf = Printf.printf
let s = string_of_int
let sm x = (s (x-1))
let sp x = (s (x+1))
let p i = "p" ^ (s i)
let neg x = "~" ^ x
let infix op x y = "(" ^ x ^ op ^ y ^ ")"
let prefix op x y = op ^ "(" ^ x ^ ", " ^ y ^ ")"
let (+:) x y = "(" ^ x ^ "+" ^ y ^ ")"
let (&:) x y = "(" ^ x ^ "&" ^ y ^ ")"
let (|:) x y = "(" ^ x ^ "|" ^ y ^ ")"
let uu x y  = "U(" ^ x ^ ", " ^ y ^ ")"
let ss x y  = "S(" ^ x ^ ", " ^ y ^ ")"
let (~<) x  = "<" ^ x 
let (~>) x  = ">" ^ x 
let array_to_string  = fun j f a -> String.concat j (Array.to_list (Array.map  f a)) 
let shuffle a = "[" ^ (array_to_string ", " (fun x->x) a) ^ "]"

let me_unimodal = [| (~<); (~>) |]
let me_bimodal = [| infix "+" |]
let f_unimodal = [| neg  |]
let f_bimodal = [| uu; ss; infix "&"; infix "|"  |]
let atoms = [|"a";"b";"c";"p";"q";"r"|]
let atoms2 = Array.of_list (List.map (fun x -> "{" ^ (String.concat ", " ((x))) ^ "}")
(powerset (Array.to_list atoms))) ;;


let rand_string bi uni atoms size =
        let _ = "" ^ (uni.(0) "") in
        let _ = "" ^ (bi.(0) "" "") in
        let uni_l = Array.length uni in
        let atoms_l = Array.length atoms in
        let bi_l  = Array.length bi in
        let rec ff size = "" ^
              match (0+size) with 
              1 -> "" ^ atoms.(Random.int atoms_l)  
              | 2 -> "" ^ uni.(Random.int uni_l) (ff(size-1))
              | _ -> "" ^ match Random.bool () with
                        true ->  uni.(Random.int uni_l) (ff(size-1))
                        | false -> let split = Random.int (size-2) in
                                let x = ff (split+1) in
                                let y = ff ((size-split)-2) in
                                "" ^ bi.(Random.int bi_l) x y
        in
        ff (size+0);;
           
let rand_formula = rand_string f_bimodal f_unimodal
let rand_me = rand_string me_bimodal me_unimodal

let iteration = (int_of_string Sys.argv.(3)) 
let size1 = (int_of_string Sys.argv.(1)) ;;
let size2 = (int_of_string Sys.argv.(2)) ;;
let _ = (Random.full_init [|iteration|]);;


printf "%s : %s\n" (rand_formula atoms size1) (rand_me atoms2 size2)
