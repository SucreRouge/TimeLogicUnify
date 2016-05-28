(* Boilerplate code for calling OCaml in the worker thread. *)
let output_buffer_ = Buffer.create 1000
let flush x=let module J = Js.Unsafe in let () = J.call 
		(J.variable "postMessage") (J.variable "self")
	 	[|J.inject (Js.string (Buffer.contents output_buffer_))|]
	 in Buffer.clear output_buffer_

let print_string = Buffer.add_string output_buffer_
let print_char = Buffer.add_char output_buffer_
let print_newline () = print_char '\n'
let print_endline s = print_string (s^"\n"); flush ()
let caml_ml_output_char = print_char
let printf fmt = Printf.bprintf output_buffer_ fmt
module Printf = struct
	include Printf
	let printf fmt = Printf.bprintf output_buffer_ fmt
end
module Sys = struct
	let char_split delim s = (*Str.split is overkill*)
		let hd = ref "" in let l = ref [] in 
		String.iter (fun c -> 
			if c = delim
			then  (l := (!hd)::(!l); hd := "")
			else hd := (!hd) ^ (String.make 1 c)
		) s;
		List.rev ((!hd)::(!l)) 
	let getenv x = List.assoc x Url.Current.arguments
	let argv = Array.of_list (char_split '\x00' (getenv "?argv"))
	let executable_name = argv.(0)
end
   
