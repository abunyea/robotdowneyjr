
type space = P1 | P2 | Grey | Empty | Void;;
type move = (int * int) list;;
type board = space array array;;


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

let rec parse_line line : space array =
	let row = Array.make 25 Void in
	let rec run_through index = 
		if index == 25 then row else
		((row.(index) <- (char_to_space line.[index])); run_through (index + 1)) in
	run_through 0;;
	
let read_input() : board = 
	let board = Array.make 17 (Array.make 25 Void) in
	  let rec run_through index = 
			if index = 17 then board else
				let line = read_line() in
				((board.(index) <- (parse_line line)); run_through (index + 1)) in
			run_through 0;;

let print_board board = 
	let print_row row =
		((Array.iter (fun x -> print_char(space_to_char x)) row); print_newline()) in
	Array.iter (fun x -> print_row x) board;;


let brd = read_input() in
print_board brd 
