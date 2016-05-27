(* Boilerplate code for calling OCaml in the worker thread. *)
let output_buffer_ = Buffer.create 1000
let flush()=(Js.Unsafe.call (Js.Unsafe.variable "postMessage")
	 (Js.Unsafe.variable "self")
	 [|Js.Unsafe.inject (Js.string (Buffer.contents output_buffer_))|];
	 Buffer.clear output_buffer_)
let stdout = ()
let stderr = ()

let print_string = Buffer.add_string output_buffer_
let print_char = Buffer.add_char output_buffer_
let print_newline = print_char '\n'
let print_endline s = print_string (s^"\n"); flush ()

(* let caml_ml_output_char c = ();; *)
let printf fmt = Printf.bprintf output_buffer_ fmt
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

let _ = print_endline (String.lowercase Sys.argv.(1))
let _ = flush ()
   
