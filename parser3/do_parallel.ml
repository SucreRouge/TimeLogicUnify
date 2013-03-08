(* #!/usr/bin/env ocaml
 (* See: http://pleac.sourceforge.net/pleac_ocaml/processmanagementetc.html*)
 #load "unix.cma";; *)

let list_remove e l = (List.fold_right (fun e2 l2 -> if e2=e then l2 else e2::l2) l []) ;;
let list_remove e l = (List.filter (fun e2 -> e2!=e) l);;
let list_remove e l = (List.filter ((!=) e) l);;


(* TODO
 * Parser Txt2tree
 *	Tree2txt
 *	Spaces? Transform?
 * Contradictions
 * Run
 * Cache
 *)

type 'a tree = { l : 'a; c : 'a tree list; };;
let test_tree = {l = "&"; c = [{l = "p"; c = []}; {l = "q"; c = []}]};;

let start_command command =
  match Unix.fork () with
    | 0 -> command ()
    (*Unix.execv command.(0) command *)
    | pid -> pid

let wait () = print_endline "Enter Wait" ; let r = Unix.wait () in print_endline "Exit wait"; r

let kill pid signal = try Unix.kill pid signal with Unix.Unix_error (Unix.ESRCH, _, _) -> (* process already killed*) () 
let killlist signal = Array.iter (fun pid -> kill pid signal)

(* Runs an array of commands in parallel. Each command is a tuple (t,f) where t
 * is the task to be run in another process and f is the finishing task to be
 * run in the same process *)
let do_commands commands timeout concurrent =
  let running = ref [] in
  let inprogress = ref true in
  let num_commands = Array.length commands in
  let killed = Array.make num_commands 0 in
  let start_times = Array.make num_commands 0.0 in
  let run_times = Array.make num_commands 0.0 in
  let pids = Array.make num_commands (-1) in
  let pid2i = Hashtbl.create num_commands in
  let next_command = ref 0 in
  let sigxcpu = 24 in
    Sys.set_signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> failwith "timeout"));
    while (!inprogress) do (
      (* while ((next_command<num_commands) and ((List.length(!running)) < concurrent)) do ( *)
      while (((!next_command)<num_commands) && ((List.length(!running)) < concurrent)) do (
        let i = (!next_command) in
        (* Here we run the actual commands *)
        let pid = start_command (fst commands.(i)) in
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
              (let (pid,status) = wait() in
                 (* Race condition? *)
                 ignore (Unix.alarm 0);
                 let endtime = Unix.gettimeofday() in
                 let i = Hashtbl.find pid2i pid in
                 print_string (String.concat "; " (List.map string_of_int (!running)));
                 run_times.(i) <- endtime -. start_times.(i);
                 (* Here we run the cleanup/finishing task *)
                 (try
                    (snd commands.(i)) run_times.(i);
                  with e ->
                    (print_endline "Exception finalising task";
                     print_endline (Printexc.to_string e);
                     Printexc.print_backtrace stderr));
                   running := list_remove i (!running);
                 Hashtbl.remove pid2i pid)
            with
                Failure "timeout" -> (
		  print_endline "timeout.";
                  (* timed out; do what you will here *)
                  let t = Unix.gettimeofday() in
                  let rec kill_out_of_time l = (
                    match l with
                        [] -> ()
                      | i::r -> if (time_remaining t i > 0.0)
                        then ()
                        else (
                          let pid = pids.(i) in
                            kill pid (if killed.(i) > 1 then Sys.sigkill else sigxcpu);
                            killed.(i) <- killed.(i) + 1;
                            if killed.(i) > 3 then (
                              print_string "Can't kill a pid, will just forget it\n";
                              running := List.tl (!running);
                            );
                            kill_out_of_time r;
                        )
                  ) in
                    kill_out_of_time (!running);
                    Unix.sleep 1
                )
	      |  Unix.Unix_error (Unix.ECHILD, _, _) -> 
                  (* clear the still-pending alarm *)
                  ignore (Unix.alarm 0);
		  print_endline "";
		  Printf.printf "No more children but %d elements remain in running\n" (List.length (!running))   ;
		  Printexc.print_backtrace stderr;
		  print_endline "";
		  running := []
              | e ->
			print_endline "Unknown exception e";
			print_endline (Printexc.to_string e);
			Printexc.print_backtrace stderr;
                	ignore (Unix.alarm 0);
			killlist Sys.sigterm pids;
			Unix.sleep 1;
			killlist Sys.sigkill pids;
		  	raise e 
                  (* clear the still-pending alarm *)
          (* propagate unexpected exception *)
          (* Should perhaps clean up running procs?*)
          )
      )
    ) done;
    run_times

(*	 ../../4mlsolver/mlsolver/bin/mlsolver.exe -pgs recursive -ve -val
 *	 ctlstar  "( p )" *)
(*
 let main x =  ignore (do_commands [|
 [|"/usr/bin/yes"; "bar"|];
 [|"/usr/bin/yes"; "foo"|];
 [|"/usr/bin/yes"; "baz"|];
 [|"/usr/bin/yes"; "BAZ"|];
 |] 1.9 3)
 let main x = ignore (do_commands [|
 [| "../../4mlsolver/mlsolver/bin/mlsolver.exe"; "-pgs"; "recursive";
 "-ve"; "-val"; "ctlstar"; "( p )" |]
 |] 1.9 3)
 let () =
 Printexc.record_backtrace true;
 try
 Printexc.print main ()
 with
 e -> Printexc.print_backtrace stdout;


 *)
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

(* vim: set noexpandtab
 * vim: set textwidth=0 *)
