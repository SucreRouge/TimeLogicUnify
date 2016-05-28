let _ = print_string (Array.fold_left (^) "" (Array.make 40 (String.lowercase (Sys.argv.(1)^"\n"))) )
