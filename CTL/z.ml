(* See: http://pleac.sourceforge.net/pleac_ocaml/processmanagementetc.html*)
#load "unix.cma";;

let list_remove e l = (List.fold_right (fun e2 l2 -> if e2=e then l2 else e2::l2) l []) ;;


let start_command command = 
  match Unix.fork () with
    | 0 ->
        Unix.execv command.(0) command
    | pid -> pid

let do_commands commands timeout concurrent=
        let running = ref [] in
        let inprogress = ref true in
        let killed = Array.length commands 0 in
        let num_commands = Array.length commands in
        let start_times = Array.make num_commands 0.0 in
        let run_times = Array.make num_commands 0.0 in
        let pids = Array.make num_commands (-1) in
        let pid2i = Hashtbl.create num_commands in
        let next_command = ref 0 in
        Sys.set_signal Sys.sigalrm
            (Sys.Signal_handle (fun _ -> failwith "timeout"));
        while (!inprogress) do (
               while (next_command < num_commands and (List.length (!running)) < concurrent) do (
                    let pid = start_command command in
                    let i = (!next_command) in
                    running := running @ [pid];
                    start_times.(i) <- Unix.gettimeofday();
                            pids.(i) <- pid;
                            Hashtbl.replace pid2i pid i;      
                            next_command := ((!next_command)+1)
               ) done;
               if (running = []) then (
                       !inprogress := false
               ) else (
                       let i = List.hd running in
                       let sleep = ceil (timeout - ((Unix.gettimeofday()) -
                       start_times.(i)))
                       ignore (Unix.alarm sleep);
                       try 
                               (let (pid,status) = Unix.wait() in
                               (* Race condition? *)
                               ignore (Unix.alarm 0);
                               let endtime = Unix.gettimeofday() in
                               let i = Hashtbl.find pid in 
                               running_times.(i) <- endtime - start_times(i);
                               running := list_remove i (!running);
                               Hashtbl.remove pid2i pid)
                       with
                               | Failure "timeout" -> (
                                (* timed out; do what you will here *)
                                let sigxcpu = 24 in
                                let sigkill = 9 in
                                Unix.kill pid (if killed.(i) > 1 then sigkill else
                                        sigxcpu);
                                killed.(i) := killed.(i) + 1;
                                if killed.(i) > 3 then (
                                        print_string "Can't kill a pid, will just forget it\n";
                                        running = List.tl running;
                                );
                                Unix.sleep 1)
                               | e ->
                                (* clear the still-pending alarm *)
                                ignore (Unix.alarm 0);
                                (* propagate unexpected exception *)
                                (* Should perhaps clean up running procs?*)
                                raise e
               )
                               
                                


               );


                   
               


        

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
(* Run a command while blocking interrupt signals. *)

