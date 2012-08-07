(* file: main.ml *)

(*
let url_decoding = Str.regexp "%[0-9][0-9]"

let decoding str =
  let str = Str.matched_string str in
  let s = String.create 4 in
  s.[0] <- '0';
  s.[1] <- 'x';
  s.[2] <- str.[1];
  s.[3] <- str.[2];
  String.make 1 (Char.chr (int_of_string s))
 
let decode str =
  Str.global_substitute url_decoding decoding str
*)
let clear_equals = fun s -> let p = String.index s '=' in String.sub s (p+1) (String.length s - p - 1)
let rec fixstr_ s i j = if j >= String.length s then String.sub s 0 i else let (si,jj)=(match s.[j] with '%' -> (char_of_int (int_of_string ("0x"^(String.sub s (j+1) 2))) ,j+3) | '+' -> (' ',j+1) | _ -> (s.[j],j+1)) in s.[i]<-si ; fixstr_ s (i+1) jj 
let fixstr = fun s -> fixstr_ (clear_equals s) 0 0


let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    let _ = Printf.printf "Content-type: text/plain\n\n" in
(*    let _ = print_string ((Sys.getenv "QUERY_STRING")^"\n\n") ; flush stdout in *)
    let _ = print_string ((fixstr (Sys.getenv "QUERY_STRING")^"\n")) ; flush stdout in
    Calc.input Lexer.token (Lexing.from_string (fixstr (Sys.getenv "QUERY_STRING"))); flush stdout;
    while true do
      Calc.input Lexer.token lexbuf
    done;
        
  with End_of_file -> exit 0
      
let _ = Printexc.print main ()
