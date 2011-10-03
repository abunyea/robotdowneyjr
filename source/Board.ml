type space = P1 | P2 | Grey | Empty | Void
type board = space array array
type move = int * int * int * int
type move_set = move list

let char_to_space ch =
	match ch with
		| ' ' -> Void
		| '0' -> Empty
		| '1' -> P1
		| '2' -> P2
		| '3' -> Grey
		| _ -> failwith "Invalid character"

let space_to_char space = 
	match space with
		| P1 -> '1'
		| P2 -> '2'
		| Grey -> 'G'
		| Empty -> 'O'
		| Void -> ' '

let copy_board board = 
	let new_board = Array.make 17 (Array.make 25 Void) in
	let rec helper index =
		if index == 17 then new_board else
		(new_board.(index) <- Array.copy board.(index); helper (index + 1)) in
	helper 0

let do_move board (x1, y1, x2, y2) =
    let new_board = copy_board board in
    let piece_to_move = new_board.(y1).(x1) in
    (new_board.(y1).(x1) <- Empty; 
    new_board.(y2).(x2) <- piece_to_move; 
    new_board)

let print_board board = 
	let print_row row =
		((Array.iter (fun x -> print_char(space_to_char x)) row); print_newline()) in
	Array.iter (fun x -> print_row x) board

let rec parse_line line : space array =
	let row = Array.make 25 Void in
	let rec run_through index = 
		if index == 25 then row else
		((row.(index) <- (char_to_space line.[index])); run_through (index + 1)) in
	run_through 0
																						
let read_input () : board = 
	let board = Array.make 17 (Array.make 25 Void) in
	  let rec run_through index = 
			if index = 17 then board else
				let line = read_line() in
				((board.(index) <- (parse_line line)); run_through (index + 1)) in
			run_through 0

let string_of_move (x1, y1, x2, y2) =
  (string_of_int x1) ^ " " ^ (string_of_int y1) ^ " " ^
  (string_of_int x2) ^ " " ^ (string_of_int y2) ^ " -1 -1"
