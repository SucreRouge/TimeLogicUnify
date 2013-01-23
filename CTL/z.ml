(* #!/usr/bin/env ocaml 
(* See: http://pleac.sourceforge.net/pleac_ocaml/processmanagementetc.html*)
#load "unix.cma";; *)

let list_remove e l = (List.fold_right (fun e2 l2 -> if e2=e then l2 else e2::l2) l []) ;;


(* TODO
 * Parser Txt2tree
 *    Tree2txt
 *       Spaces? Transform?
 * Contradictions
 * Run
 * Cache
 *)

type 'a tree = { l : 'a; c : 'a tree list; };;
let test_tree = {l = "&"; c = [{l = "p"; c = []}; {l = "q"; c = []}]};;

let start_command command = 
  match Unix.fork () with
    | 0 ->
        Unix.execv command.(0) command
    | pid -> pid

let do_commands commands timeout concurrent=
        let running = ref [] in
        let inprogress = ref true in
        let num_commands = Array.length commands in
        let killed = Array.make num_commands 0 in
        let start_times = Array.make num_commands 0.0 in
        let run_times = Array.make num_commands 0.0 in
        let pids = Array.make num_commands (-1) in
        let pid2i = Hashtbl.create num_commands in
        let next_command = ref 0 in
        Sys.set_signal Sys.sigalrm
            (Sys.Signal_handle (fun _ -> failwith "timeout"));
        while (!inprogress) do (
               (* while ((next_command<num_commands) and ((List.length(!running)) < concurrent)) do ( *)
               while (((!next_command)<num_commands) && ((List.length(!running)) < concurrent)) do (
                    let i = (!next_command) in
                    let pid = start_command commands.(i) in
                    running := (!running) @ [i];
                    start_times.(i) <- Unix.gettimeofday();
                            pids.(i) <- pid;
                            Hashtbl.replace pid2i pid i;      
                            next_command := ((!next_command)+1)
               ) done;
               if ((!running) = []) then (
                       inprogress := false
               ) else (
                       let i = List.hd (!running) in
                       let time_remaining t i = (timeout -. (t -.  start_times.(i))) in
                       let sleepi = int_of_float (ceil (time_remaining (Unix.gettimeofday()) i)) in
                       (*ignore (Unix.alarm sleep);*)
                       ignore (Unix.alarm (if sleepi > 0 then sleepi else 1));
                       (*ignore (Unix.alarm 1);  test *)
                       ( try 
                               (*if sleep > 0 then () else failwith "timeout"; *)
                               (let (pid,status) = Unix.wait() in
                               (* Race condition? *)
                               ignore (Unix.alarm 0);
                               let endtime = Unix.gettimeofday() in
                               let i = Hashtbl.find pid2i pid in 
                               run_times.(i) <- endtime -. start_times.(i);
                               running := list_remove i (!running);
                               Hashtbl.remove pid2i pid)
                       with
                               Failure "timeout" -> (
                                (* timed out; do what you will here *)
                                let sigxcpu = 24 in
                                let sigkill = 9 in
                                let t = Unix.gettimeofday() in
                                let rec kill_out_of_time l = (
                                        match l with 
                                        [] -> () 
                                        | i::r -> if (time_remaining t i > 0.0)
                                        then ()
                                        else (
                                        
                                let pid = pids.(i) in
                                Unix.kill pid (if killed.(i) > 1 then sigkill else
                                        sigxcpu);
                                killed.(i) <- killed.(i) + 1;
                                if killed.(i) > 3 then (
                                        print_string "Can't kill a pid, will just forget it\n";
                                        running := List.tl (!running);
                                );
                                kill_out_of_time r;
                                )
                                ) in 
                                kill_out_of_time (!running);
                                Unix.sleep 1)
                               | e ->
                                (* clear the still-pending alarm *)
                                ignore (Unix.alarm 0)
                                (* propagate unexpected exception *)
                                (* Should perhaps clean up running procs?*)
                       )
               )
        ) done;
        run_times
        
          

let main x =  ignore (do_commands [|
        [|"/usr/bin/yes"; "bar"|];
        [|"/usr/bin/yes"; "foo"|];
        [|"/usr/bin/yes"; "baz"|];
        [|"/usr/bin/yes"; "BAZ"|];
        |] 1.9 3)

let () =
        Printexc.record_backtrace true;
        try
                Printexc.print main ()
        with
                e -> Printexc.print_backtrace stdout;
                   
               
(*

        

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
*)
