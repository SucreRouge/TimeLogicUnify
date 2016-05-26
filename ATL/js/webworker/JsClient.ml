(* Boilerplate code for calling OCaml in the worker thread. *)
let js_object = Js.Unsafe.variable "Object"
let js_handler = jsnew js_object ()
let postMessage = Js.Unsafe.variable "postMessage"

let print_string s = Js.Unsafe.call postMessage (Js.Unsafe.variable "self") [|Js.Unsafe.inject (Js.string s)|]
let print_endline s = (print_string (s^"\n"));;
let print_newline () = (print_string "\n");;
let caml_ml_output_char c = ();;
let print_char c = ();;
let printf fmt = 
	let output_buffer = Buffer.create 1000 in
	Printf.bprintf output_buffer fmt;;
let url_argument x = List.assoc x Url.Current.arguments
let url_argument x = List.assoc x [("arg1","XYA")];;

let rec arg_str l = match l with 
	(a,b)::tl -> "["^a^","^b^"]"^(arg_str tl)
	| _ -> ""

let _ = print_endline "Goodbye"
   
