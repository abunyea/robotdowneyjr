open Print

type space = P1 | P2 | Grey | Empty | Void
type board = space array array
type move = int * int * int * int * int * int
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

let player_of_int i =
  match i with
  | 1 -> P1
  | 2 -> P2
  | _ -> failwith "Can't parse player, must be 1 or 2" 


let toggle_player player =
  match player with
  | P1 -> P2
  | P2 -> P1
	| _ -> failwith "Invalid input to toggle_player"

let copy_board board = 
	let new_board = Array.make 17 (Array.make 25 Void) in
	let rec helper index =
		if index == 17 then new_board else
		(new_board.(index) <- Array.copy board.(index); helper (index + 1)) in
	helper 0

(* Takes a move in the format*)
(* (x1, y1, x2, y2) and executes it,*)
(* returning a new board *)
let do_move board (x1, y1, x2, y2, x3, y3) =
    let new_board = copy_board board in
		if x3 = -1 then () else new_board.(y3).(x3) <- Grey;
    let piece_to_move = new_board.(y1).(x1) in
    (new_board.(y1).(x1) <- Empty; 
    new_board.(y2).(x2) <- piece_to_move; 
    new_board)

let print_board board = 
	let print_row row =
		((Array.iter (fun x -> prerr_char(space_to_char x)) row); prerr_newline()) in
	Array.iter (fun x -> print_row x) board

let print_board_standard board = 
	let print_row row =
		((Array.iter (fun x -> print_char(space_to_char x)) row); print_newline()) in
	Array.iter (fun x -> print_row x) board
let string_of_move (x1, y1, x2, y2, x3, y3) =
  (string_of_int y1) ^ " " ^ (string_of_int x1) ^ " " ^
  (string_of_int y2) ^ " " ^ (string_of_int x2) ^ " " ^
	(string_of_int y3) ^ " " ^ (string_of_int x3)

(* Returns the coordinates of the players home positions in the form*)
(* (y, x)               *)
(*    /\     \ p1 /     *)
(*   /  \     \  /      *)
(*  / p2 \     \/       *)
let get_home_coords player = 
	if player = P1 then [(16, 12); (15, 11); (15, 13);
		(14, 10); (14, 12); (14, 14); (13, 9); (13, 11); (13, 13); (13, 15)]
	else
		[(0,12); (1, 11); (1, 13); (2, 10); (2, 12); (2, 14);
		 (3, 9); (3, 11); (3, 13); (3, 15)]

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
	let (ours, non_empty) = List.fold_left (fun (ours, non_empty) piece -> if piece = winning_piece then (ours + 1, non_empty + 1) else (if piece = Empty then (ours, non_empty) else (ours, non_empty + 1))) (0, 0) home in
	(non_empty = 10 && ours >= 5)
	
let rec build_piece_list board player bound1 bound2 pieces = 
    if bound1 = 16 && bound2 = 24 then pieces 
    else if bound2 = 24 then 
      (if board.(bound1).(bound2) = player 
					then (build_piece_list board player (bound1+1) 0 
							((bound2,bound1)::pieces))
        	else build_piece_list board player (bound1+1) 0 pieces)
       else 
          (if board.(bound1).(bound2) = player 
					 then (build_piece_list board player (bound1) (bound2+1) 
						 ((bound2,bound1)::pieces))
           else build_piece_list board player (bound1) (bound2+1) pieces)
					


let available_moves b player = 
  (*Step list of a piece is all of the possible places it can step to *)
  let step_list (x,y) : move list= 
    List.flatten([(if x+2 < 25 && b.(y).(x+2) = Empty then [(x,y,x+2,y, -1, -1)] else []);
      (if x-2 >= 0 && b.(y).(x-2) = Empty then [(x,y,x-2,y, -1, -1)] else []);
     
	(if player = P1 then (
	  (if x-1 >= 0 && y-1 >= 0 && b.(y-1).(x-1) = Empty then [(x,y,x-1,y-1, -1, -1)] else []);
      (if x+1 < 25 && y-1 >= 0 && b.(y-1).(x+1) = Empty then [(x,y,x+1,y-1, -1, -1)] else []))
	else (
      (if x+1 < 25 && y+1 < 17 && b.(y+1).(x+1) = Empty then [(x,y,x+1,y+1, -1, -1)] else []);
      (if x-1 >= 0 && y+1 < 17 && b.(y+1).(x-1) = Empty then [(x,y,x-1,y+1, -1, -1)] else [])))]) in
  (*Jump List of a piece is all of the possible places it can jump to*)
  (* Inputs: a coordinate pair (x,y) and a list of coordinates not to consider*)
  (* so as to avoid cycles *)
  let rec jump_list lst (x,y) (u,v) = 
    (if (x+2 < 25 && b.(y).(x+2) <> Void && b.(y).(x+2) <> Empty 
      && not(List.exists (fun (a,b) -> a=x+4 && b=y) lst) 
             && x+4 < 25 && b.(y).(x+4)=Empty)
      then (u,v,x+4, y, -1, -1)::(jump_list ((x,y)::lst) (x+4,y) (u,v)) else [])@
     (if (x-2 >= 0 && b.(y).(x-2) <> Void && b.(y).(x-2) <> Empty 
      && not(List.exists (fun (a,b) -> a=x-4 && b=y) lst) && x-4 >= 0 && b.(y).(x-4)=Empty)
      then (u,v,x-4,y, -1, -1)::(jump_list ((x,y)::lst) (x-4,y) (u,v)) else [])@
     (if (x+2 < 25 && y+2 < 17 && b.(y+1).(x+1) <> Void && b.(y+1).(x+1) <> Empty 
      && not(List.exists (fun (a,b) -> a=x+2 && b=y+2) lst) && b.(y+2).(x+2)=Empty)
      then (u,v,x+2,y+2, -1, -1)::(jump_list ((x,y)::lst) (x+2,y+2) (u,v)) else [])@
     (if (x-2 >= 0 && y+2 < 17 && b.(y+1).(x-1) <> Void && b.(y+1).(x-1) <> Empty 
      && not(List.exists (fun (a,b) -> a=x-2 && b=y+2) lst) && b.(y+2).(x-2)=Empty)
      then (u,v,x-2,y+2, -1, -1)::(jump_list ((x,y)::lst) (x-2,y+2) (u,v)) else [])@
     (if (x-2 >= 0 && y-2 >= 0 && b.(y-1).(x-1) <> Void && b.(y-1).(x-1) <> Empty 
      && not(List.exists (fun (a,b) -> a=x-2 && b=y-2) lst) && b.(y-2).(x-2)=Empty)
      then (u,v,x-2,y-2, -1, -1)::(jump_list ((x,y)::lst) (x-2,y-2) (u,v)) else [])@
     (if (x+2 < 25 && y-2 >= 0 && b.(y-1).(x+1) <> Void && b.(y-1).(x+1) <> Empty 
      && not(List.exists (fun (a,b) -> a=x+2 && b=y-2) lst) && b.(y-2).(x+2)=Empty)
      then (u,v,x+2,y-2, -1, -1)::(jump_list ((x,y)::lst) (x+2,y-2) (u,v)) else []) in
	(List.fold_left (fun acc x -> (jump_list [] x x)@acc) 
                          [] (build_piece_list b player 0 0 [])) @
     (List.fold_left (fun acc x -> (step_list x)@acc) 
                        [] (build_piece_list b player 0 0 []))
      

let ordered_available_moves b player = 
	let (x0, y0) = if player = P1 then (12,0) else (12,16) in
	let rec take lst num = 
		if num >= List.length lst then lst else
			if num = 0 then [] else
				(List.hd lst)::(take (List.tl lst) (num - 1)) in
	let min_v_dist move1 move2 = 
		let (_, _, _, y1, _, _) = move1 in
		let (_, _, _, y2, _, _) = move2 in
		let abs1 = abs(y1 - y0) in
		let abs2 = abs(y2 - y0) in
		if abs1 > abs2 then 1 else (if abs1 < abs2 then -1 else 0) in
	take (List.sort min_v_dist (available_moves b player)) 20
	


(* Generates random board with n pieces in the opponents endzone 
let random_endgame_state endzone_pieces = 
	let board = Array.make 17 (Array.make 25 Void) in
	let rec run_through row_num =
		if row_num = 17 then () else
			(board.(row_num) <- Array.make 25 Void);
			run_through (row_num + 1); in
	run_through 0;
	let home_coords = get_p2_home in 
	let put_home_piece pieces_put = 
		let (y, x) = List.nth home_coords Random.int 0 10 *)
	
	
let print_movelist lst =
	let print_func (a, b, c, d, e, f) = 
		let soi = string_of_int in
		let str = (soi b) ^ ", " ^ (soi a) ^ ", " ^ (soi d) ^ ", " ^ (soi c) ^ ", " ^ (soi f) ^ ", " ^ (soi e) in
		prerr_endline str in
  List.iter print_func lst

