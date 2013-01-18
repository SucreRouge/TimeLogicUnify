(* See: http://pleac.sourceforge.net/pleac_ocaml/processmanagementetc.html*)
#load "unix.cma";;

let list_remove e l = (List.fold_right (fun e2 l2 -> if e2=e then l2 else e2::l2) l []) ;;


let start_command command = 
        

let do_commands commands timeout concurrent=
        let running = ref [] in
        let inprogress = ref true in
        let num_commands = Array.length commands in
        let start_times = Array.make num_commands 0.0 in
        let pids = Array.make num_commands (-1) in
        let next_command = ref 0 in
        while (!inprogress) do (
               while ((List.length running) < concurrent) do (
                        
                       )


        

let () =
  Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle (fun _ -> failwith "timeout"));

  ignore (Unix.alarm 1);
  try
    (* long-time operations here *)
    while true do (print_char 'y') done;
    ignore (Unix.alarm 0)
  with
    | Failure "timeout" ->
        (* timed out; do what you will here *)
        ()
    | e ->
        (* clear the still-pending alarm *)
        ignore (Unix.alarm 0);
        (* propagate unexpected exception *)
        raise e
