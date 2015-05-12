let print_string s = ()
let print_endline s = ()
let print_newline = ()
(*let caml_ml_output_char c = ()*)
let print_char c = ();;

let bool2str b = if b then "Y" else "n";;
let output_buffer = Buffer.create 1000;;
let printf fmt = Printf.bprintf output_buffer fmt;;

let () =
  begin
    printf "Foo";
    Dom_html.window##alert (Js.string ("Alert window from ocaml" ^ (Js.to_string (Dom_html.window##location##search))));
    Dom_html.window##alert (Js.string ("Alert window from ocaml" ^ (Js.to_string (Dom_html.window##location##search))));
  end
