
type space = P1 | P2 | Grey | Empty | Void;;
type move = int * int * int * int (* (x1, y1, x2, y2) *)
type board = space array array;;
type player = Player1 | Player2



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
	
let copy_board board = 
	let new_board = Array.make 17 (Array.make 25 Void) in
	let rec helper index =
		if index == 17 then new_board else
		((new_board.(index) <- Array.copy board.(index)); helper (index + 1)) in
	helper 0;;

let do_move board (x1, y1, x2, y2) : board =
	let new_board = copy_board board in
	let piece_to_move = new_board.(y1).(x1) in
	((new_board.(y1).(x1) <- Empty); (new_board.(y2).(x2) <- piece_to_move); new_board);;

let rec parse_line line : space array =
	let row = Array.make 25 Void in
	let rec run_through index = 
		if index == 25 then row else
		((row.(index) <- (char_to_space line.[index])); run_through (index + 1)) in
	run_through 0;;
	
let read_board_input() : board = 
	let board = Array.make 17 (Array.make 25 Void) in
	  let rec run_through_board index = 
			if index = 17 then board else
				let line = read_line() in
				((board.(index) <- (parse_line line)); run_through_board (index + 1)) in
			(run_through_board 0)

let read_other_input() : int * int * int * int * int * int =
	let line1 = read_line() in
	let line2 = read_line() in
	let numbers = Str.split (Str.regexp_string " ") line1 in
	let (time1, time2, grey1, grey2, player) = 
		match numbers with 
			| a::b::c::d::e::[] -> (int_of_string(a), int_of_string(b), int_of_string(c), int_of_string(d), int_of_string(e))
			| _ -> (0, 0, 0, 0, 0)
			in
	(time1, time2, grey1, grey2, player, int_of_string(line2))
	
let read_initial_input() =
	let board = read_board_input() in
	let (time1, time2, grey1, grey2, player, us) = read_other_input() in
	(board, time1, time2, grey1, grey2, player, us)

let print_board board = 
	let print_row row =
		((Array.iter (fun x -> print_char(space_to_char x)) row); print_newline()) in
	Array.iter (fun x -> print_row x) board;;

let get_possible_moves board : move list = [];;

(* Returns a random move out of the move_set*)
let random_bot move_set = 
	let len = List.length move_set in
	let index = Random.int len in
	List.nth move_set index

let get_p2_home board = 
		[board.(0).(12); 
		board.(1).(11); board.(1).(13); 
		board.(2).(10); board.(2).(12); board.(2).(14);
		board.(3).(9); board.(3).(11); board.(3).(13); board.(3).(15)];;
		
let get_p1_home board = 
		[board.(16).(12); 
		board.(15).(11); board.(15).(13); 
		board.(14).(10); board.(14).(12); board.(14).(14);
		board.(13).(9); board.(13).(11); board.(13).(13); board.(13).(15)];;
		
let has_won board player =
	let (home, winning_piece) = 
		if player = Player1 then 
			(get_p2_home board, P1)
		else (get_p1_home board, P2) in
	List.length (List.filter (fun x -> x = winning_piece) home) >= 5;;
		
(*
let minimax (board : board) (depth : int) : move =
	let rec mini_max board depth = 
		let possible_moves = get_possible_moves board in
		match (depth, possible_moves) with
			| (0, _ ) 
			| (_, []) -> evaluate board
			| _ -> List.fold_left max (List.map (fun f x -> mini_min (do_move board x) (depth -1)) possible_moves)
	and mini_min board_depth = 
		let possible_moves = get_possible_moves board in
		match (depth, possible_moves) with
			| (0, _ ) 
			| (_, []) -> evaluate board
			| _ -> List.fold_left min (List.map (fun f x -> mini_min (do_move board x) (depth -1)) possible_moves)
	 in mini_max board MAX_SEARCH_DEPTH;;

	*)
	
