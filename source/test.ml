
type space = P1 | P2 | Grey | Empty | Void;;
type board = space list list;;


let char_to_space ch =
	match ch with
		| ' ' -> Void
		| '0' -> Empty
		| '1' -> P1
		| '2' -> P2
		| '3' -> Grey
		| _ -> failwith "Invalid character";;

let space_to_char space = 
	match space with
		| P1 -> '1'
		| P2 -> '2'
		| Grey -> 'G'
		| Empty -> 'O'
		| Void -> ' ';;
		
let rec parse_line line : space list =
	let rec run_through index = 
		if index == 25 then [] else
		(char_to_space line.[index])::(run_through (index + 1)) in
	List.rev (run_through 0);;
	
let read_input() : board = 
		let rec helper board line_number = 
			if line_number == 17 then List.rev board else
			let line = read_line() in
			helper ((parse_line line)::board) (line_number + 1) in
		helper [] 0;;
		
let print_board board = 
	let rec helper row = 
		match row with
			| h::t -> ((List.iter (fun a -> print_char(space_to_char a)) h);
			 print_newline(); helper t)
			| _ -> () in
		helper board;;

let brd = read_input() in
print_board brd 
