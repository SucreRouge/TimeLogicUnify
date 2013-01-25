let printf=Printf.printf

let reverse s_ =
	let s = String.copy s_ in 
	let last_pos = (String.length s) - 1  in
	for i = 0 to (last_pos / 2)
	do
		let j = last_pos - i in
		let (c,d) = (s.[i],s.[j]) in
		s.[i] <- d ; s.[j] <- c
	done ; s
		
let fix_braces str = 
	let openb_  = "{([" in
	let closeb_ = "})]" in
	let stack = ref [] in
	let need_rev = ref false in
	let fix openb closeb s = (
		let out = Buffer.create (2 * String.length str) in 
		let add = (Buffer.add_char out) in
		String.iter ( fun ch ->
			( try 
				let brace_type = String.index openb ch in
				stack := brace_type::(!stack) ;
			with Not_found -> try
				let brace_type = String.index closeb ch in
				while (!stack != [] && List.hd (!stack) != brace_type)
				do 
					let missing_close = closeb.[List.hd (!stack)] in 
					printf "Added missing `%c' prior to `%c'\n" missing_close ch;
					add missing_close ;
					stack := List.tl (!stack) 
				done ;
				if (!stack == [])
				then	need_rev := true
					(*let missing_open = openb.[brace_type] in 
					printf "Added missing `%c'  to beginning of string\n" missing_close ch;
					(*printf "Removed redundant `%c'\n" ch*)
					missing_open_braces := ' missing_open_braces + 
						*)
				else
					stack := List.tl (!stack) 
			with Not_found -> () 
			) ; add ch )  s;
		while (!stack != [])
		do 
			let missing_close = closeb.[List.hd (!stack)] in 
			printf "Added missing `%c'\n" missing_close;
			add missing_close ;
			stack := List.tl (!stack) 
		done ;
		Buffer.contents out) in
	let fixed1 = fix openb_ closeb_ str in
	let final_output = 
		if (!need_rev)
		then ( 
			let fixed2 = fix closeb_ openb_ (reverse fixed1) in
			(*printf "outr2: %s\n" fixed2;*)
			reverse fixed2
		) else 
			fixed1 
	in
	if (str = final_output) 
	then ()
	else printf "After adding missing brackets, \n\t\"%s\"\nbecomes\n\t\"%s\"\n" str final_output ;
	flush stdout;
	final_output;
;;

