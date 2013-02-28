#load "str.cma";;

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


let url_encoding = Str.regexp "[^0-9a-zA-Z]" 

let encoding str =
  let str = Str.matched_string str in
  ("%" ^ Printf.sprintf  "%x" (Char.code (String.get str 0)))
        
let encode str = Str.global_substitute url_encoding encoding str;;

let _ = print_string (encode "((p&q)");;
let _ = print_string (decode (encode "((p&q)"));;

