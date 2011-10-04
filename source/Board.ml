type space = P1 | P2 | Grey | Empty | Void
type board = space array array
type move = int * int * int * int
type move_set = move list
type player = Player1 | Player2

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

let player_of_int i =
  match i with
  | 1 -> Player1
  | 2 -> Player2
  | _ -> failwith "Can't parse player, must be 1 or 2" 

let toggle_player player =
  match player with
  | Player1 -> Player2
  | Player2 -> Player1

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
		((Array.iter (fun x -> prerr_char(space_to_char x)) row); prerr_newline()) in
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
  (string_of_int y1) ^ " " ^ (string_of_int x1) ^ " " ^
  (string_of_int y2) ^ " " ^ (string_of_int x2) ^ " -1 -1"

(* Returns the coordinates of the players home positions in the form*)
(* (y, x) *)
let get_home_coords player = 
	if player = P1 then [(16, 2); (15, 11); (15, 13);
		(14, 10); (14, 12); (14, 14); (13, 9); (13, 11); (13, 13); (13, 15)]
	else
		[(0,12); (15, 11); (15, 13); (14, 10); (14, 12); (14, 14);
		 (13, 9); (13, 11); (13, 13); (13, 15)]

(* Returns the pieces in the players home position. Note that if you want to*)
(* check *)
let get_home_pieces board player = 
	let coords = get_home_coords player in
	List.fold_left (fun acc (y, x) -> board.(y).(x)::acc) [] coords
	
let get_p1_home board = 
	[board.(16).(12); 
	board.(15).(11); board.(15).(13); 
	board.(14).(10); board.(14).(12); board.(14).(14);
	board.(13).(9); board.(13).(11); board.(13).(13); board.(13).(15)]

let get_p2_home board = 
	[board.(0).(12); 
	board.(1).(11); board.(1).(13); 
	board.(2).(10); board.(2).(12); board.(2).(14);
	board.(3).(9); board.(3).(11); board.(3).(13); board.(3).(15)]

let has_won board player =
	let (home, winning_piece) = 
		if player = P1 then 
			(get_home_pieces board P2, P1)
		else (get_home_pieces board P1, P2) in
	List.length (List.filter (fun x -> x = winning_piece) home) >= 5

let available_moves b player = 
  let rec build_piece_list i1 i2 p = 
    if i1 = 16 && i2 = 24 then p 
    else if i2 = 24 then 
      (if b.(i1).(i2) = player then (prerr_endline ((string_of_int i1) ^ "," ^ (string_of_int i2)); build_piece_list (i1+1) 0 ((i2,i1)::p))
        else build_piece_list (i1+1) 0 p)
       else 
         (if b.(i1).(i2) = player then (prerr_endline ((string_of_int i1) ^ "," ^ (string_of_int i2)); build_piece_list (i1) (i2+1) ((i2,i1)::p))
          else build_piece_list (i1) (i2+1) p) in
  (*Step list of a piece is all of the possible places it can step to *)
  let step_list (x,y) = 
    List.flatten([(if x+2 < 17 && b.(y).(x+2) = Empty then [(x,y,x+2,y)] else []);
      (if x-2 >= 0 && b.(y).(x-2) = Empty then [(x,y,x-2,y)] else []);
      (if x-1 >= 0 && y-1 >= 0 && b.(y-1).(x-1) = Empty then [(x,y,x-1,y-1)] else []);
      (if x+1 < 17 && y-1 >= 0 && b.(y-1).(x+1) = Empty then [(x,y,x+1,y-1)] else []);
      (if x+1 < 17 && y+1 < 25 && b.(y+1).(x+1) = Empty then [(x,y,x+1,y+1)] else []);
      (if x-1 >= 0 && y+1 < 25 && b.(y+1).(x-1) = Empty then [(x,y,x-1,y+1)] else [])]) in
  (*Jump List of a piece is all of the possible places it can jump to*)
  (* Inputs: a coordinate pair (x,y) and a list of coordinates not to consider*)
  (* so as to avoid cycles *)
  let rec jump_list lst (x,y) (u,v) = 
    (if (x+2 < 25 && b.(y).(x+2) <> Void && b.(y).(x+2) <> Empty 
      && not(List.exists (fun (a,b) -> a=x+4 && b=y) lst) 
             && x+4 < 25 && b.(y).(x+4)=Empty)
      then (u,v,x+4,y)::(jump_list ((x,y)::lst) (x+4,y) (u,v)) else [])@
     (if (x-2 >= 0 && b.(y).(x-2) <> Void && b.(y).(x-2) <> Empty 
      && not(List.exists (fun (a,b) -> a=x-4 && b=y) lst) && x-4 >= 0 && b.(y).(x-4)=Empty)
      then (u,v,x-4,y)::(jump_list ((x,y)::lst) (x-4,y) (u,v)) else [])@
     (if (x+2 < 25 && y+2 < 17 && b.(y+1).(x+1) <> Void && b.(y+1).(x+1) <> Empty 
      && not(List.exists (fun (a,b) -> a=x+2 && b=y+2) lst) && b.(y+2).(x+2)=Empty)
      then (u,v,x+2,y+2)::(jump_list ((x,y)::lst) (x+2,y+2) (u,v)) else [])@
     (if (x-2 >= 0 && y+2 < 17 && b.(y+1).(x-1) <> Void && b.(y+1).(x-1) <> Empty 
      && not(List.exists (fun (a,b) -> a=x-2 && b=y+2) lst) && b.(y+2).(x-2)=Empty)
      then (u,v,x-2,y+2)::(jump_list ((x,y)::lst) (x-2,y+2) (u,v)) else [])@
     (if (x-2 >= 0 && y-2 >= 0 && b.(y-1).(x-1) <> Void && b.(y-1).(x-1) <> Empty 
      && not(List.exists (fun (a,b) -> a=x-2 && b=y-2) lst) && b.(y-2).(x-2)=Empty)
      then (u,v,x-2,y-2)::(jump_list ((x,y)::lst) (x-2,y-2) (u,v)) else [])@
     (if (x+2 < 25 && y-2 >= 0 && b.(y-1).(x+1) <> Void && b.(y-1).(x+1) <> Empty 
      && not(List.exists (fun (a,b) -> a=x+2 && b=y-2) lst) && b.(y-2).(x+2)=Empty)
      then (u,v,x+2,y-2)::(jump_list ((x,y)::lst) (x+2,y-2) (u,v)) else []) in
     (List.fold_left (fun acc x -> (step_list x)@acc) 
                        [] (build_piece_list 0 0 []))
      @(List.fold_left (fun acc x -> (jump_list [] x x)@acc) 
                          [] (build_piece_list 0 0 []))

let print_movelist lst =
  List.iter (fun (a, b, c, d) -> (prerr_endline ((string_of_int a) ^ ", " ^ (string_of_int b) ^ ", " ^ (string_of_int c) ^ ", " ^ (string_of_int d)))) lst

