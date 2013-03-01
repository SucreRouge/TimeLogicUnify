(* From: http://www2.lib.uchicago.edu/keith/ocaml-class/complete.html *)
let cat filename =
  let chan = open_in filename in
  let size = 4 * 1024 in
  let buffer = String.create size in
  let eof = ref false in
    while not !eof do
      let len = input chan buffer 0 size in
	if len > 0
	then print_string (String.sub buffer 0 len)
	else eof := true
    done
