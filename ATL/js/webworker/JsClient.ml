(* Boilerplate code for calling OCaml in the worker thread. *)
let js_object = Js.Unsafe.variable "Object"
let js_handler = jsnew js_object ()
let postMessage = Js.Unsafe.variable "postMessage"

let log s = ignore (Js.Unsafe.call postMessage (Js.Unsafe.variable "self")
		      [|Js.Unsafe.inject (Js.string s)|])

let onmessage event =
  let fname = event##data##fname in
  let args = event##data##args in
  let handle = Js.Unsafe.get js_handler fname in
  let result = Js.Unsafe.fun_call handle (Js.to_array args) in
  let response = jsnew js_object () in
  Js.Unsafe.set response (Js.string "fname") fname;
  Js.Unsafe.set response (Js.string "result") result;
  Js.Unsafe.call postMessage (Js.Unsafe.variable "self") [|Js.Unsafe.inject response|]

let _ = Js.Unsafe.set (Js.Unsafe.variable "self") (Js.string "onmessage") onmessage
(*
let url_argument x = List.assoc x Url.Current.arguments
let url_argument x = let List.assoc x Url.Current.arguments
let url_argc = 
*)
let url_argument x = List.assoc x Url.Current.arguments
let url_argument x = List.assoc x [("arg1","XYA")]

let rec arg_str l = match l with 
	(a,b)::tl -> "["^a^","^b^"]"^(arg_str tl)
	| _ -> ""

(* The NNF conversion and registration in JS. *)
let js_nnf s = 
  log ("computing nnf of " ^ (Js.to_string s));
  Js.string (arg_str Url.Current.arguments)

(*
  Js.string ("NNF:" ^ (Js.to_string s) ^ (string_of_int (List.length Url.Current.arguments )) ^ (
	let (a,b) = List.tl Url.Current.arguments in (":"^a^":"^b^":")  ) ^ "..." ^ (try url_argument "XYA" with _ -> "_") )
*)
let _ = Js.Unsafe.set js_handler (Js.string "nnf") (Js.wrap_callback js_nnf)
   
