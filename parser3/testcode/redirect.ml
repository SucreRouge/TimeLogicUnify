#load "Unix.cma"
let outfile = (Unix.openfile "test.out" [Unix.O_CREAT; Unix.O_WRONLY] 0o644)
let _ = Unix.dup2 outfile Unix.stdout;;
(*let _ = Unix.dup2 Unix.stderr Unix.stdout;;*)
let _ =Printf.printf "Redirected\n";;

