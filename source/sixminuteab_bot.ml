open Board
open Print
open State
open JohnRocker_bot
open Greedy_bot
open KeithHernandez_bot
open Baby_bot
open Minimax_bot
open Manhattan_Bot

(* minimum distance before switching to minimax *)
let middle_distance = 2
(* Number of their marbles that must have passed all of ours
   before we switch to ending bot*)
let num_ending = 8

let get_grey_row player = if player = P1 then 5 else 11 

(* Criteria for switching to john rocker bot:*)
(*  all pieces in last 6 rows. At least 5 in last 4 *)

let build_piece_list_a board player = 
	build_piece_list board player 0 0 [];;

let num_moves = ref 0
let middle = ref false
let use_grey = ref false
let ending = ref false
let closer = ref false

let check_grey state player : unit = 
	let remaining = if player = P1 then state.grey_remain_1 else state.grey_remain_2 in
	if remaining = 0 then use_grey:= false else
	(if not !use_grey then
		let grey_row = get_grey_row player in
		let our_pieces = build_piece_list_a state.board player in
		let their_pieces = build_piece_list_a state.board (toggle_player player) in
		let ours_before = List.fold_left (fun acc (x, y) -> if (player = P1 && y > 12) then (acc + 1) else if (player = P2 && y < 12) then (acc + 1) else acc) 0 our_pieces in
		let theirs_after = List.fold_left (fun acc (x, y) -> if (player = P1 && y >= grey_row) then (acc + 1) else if (player = P2 && y <= grey_row) then (acc + 1) else acc) 0 their_pieces in
		if theirs_after = 10 (* || (theirs_after + ours_before >= 18) *) then
			(prerr_endline "Time to place some greys";
			 use_grey:= true))
		
		
(* checks distance between furthest marbles on each side,
 * updates middle *)
let check_middle state : unit = 
  if not !middle then 
    let p1_list = build_piece_list_a state.board P1 in
    let p2_list = build_piece_list_a state.board P2 in
    let furthest_p1 = List.fold_left (fun a (_, x) -> min a x) 19 p1_list in
    let furthest_p2 = List.fold_left (fun a (_, x) -> max a x) (-1) p2_list in
    middle := abs (furthest_p1 - furthest_p2) <= middle_distance;
		if !middle then (prerr_endline ("Switch to alphabeta at move " ^ (string_of_int !num_moves))) else ()

(* updates ending if an ending state is reached *)
let check_ending state : unit = 
  if not !ending then 
    let p1_list = build_piece_list_a state.board P1 in
    let p2_list = build_piece_list_a state.board P2 in
    let our_list = if state.player = P1 then p1_list else p2_list in
    let their_list = if state.player = P1 then p2_list else p1_list in
    let our_last = List.fold_left 
      (fun a (_, x) -> (if state.player = P1 then max else min) a x) 
        (if state.player = P1 then 0 else 19) our_list in
    let num_behind = List.fold_left
      (fun a (_, y) -> if (if state.player = P1 then (>) else (<)) 
         y our_last then a+1 else a)
       0 their_list in
    ending := num_behind >= num_ending;
		if !ending then (prerr_endline ("Switch to end_bot at move " ^ (string_of_int !num_moves))) else ()
		
let place_grey (x1, y1, x2, y2, x3, y3) state player =
	let grey_row = get_grey_row player in
	let possible_spots = List.filter (fun (x, y) -> state.board.(y).(x) = Empty) [(11, grey_row); (13, grey_row); (9, grey_row); (15, grey_row)] in
	match possible_spots with
		| [] -> (x1, y1, x2, y2, x3, y3)
		| (x, y)::t -> (x1, y1, x2, y2, x, y)
	

let check_closer state player : unit =
	if not !closer then
	let our_pieces = build_piece_list state.board player 0 0 [] in
	let y_dist = List.map (fun (x, y) -> if player = P1 then y else 16 - y) our_pieces in
	let (num_less_four, num_less_six) = List.fold_left (fun (less_four, less_six) y_dist -> if y_dist < 4 then (less_four + 1, less_six + 1) else (if y_dist < 6 then (less_four, less_six + 1) else (less_four, less_six))) (0, 0) y_dist in
	if (num_less_four >= 6 && num_less_six = 10) then (
		prerr_endline "Bringing in the lefty.."; closer:= true) else ()
	
let build_sixminuteab_bot start_bot middle_bot1 middle_bot2 end_bot closer_bot state player =
	
	let time_left = if player = P1 then state.time1 else state.time2 in
	prerr_endline (string_of_int time_left);
	incr num_moves;
  check_middle state;
	
	(* check_grey state player; *)
	check_ending state;
	check_closer state player;
	let bot = (
		match (!use_grey, !middle, !ending, !closer) with
		  | (true, _, _, _) -> grey_alphabeta_bot 
			| (false, false, false, false) -> start_bot
			| (_, true, false, false) -> if time_left > 190000 then middle_bot1 else middle_bot2
			| (_, _, true, false) -> end_bot
			| (_, _, _, true) -> closer_bot) in
	bot state player

let sixminutebeam state player =
	build_sixminuteab_bot (ids_bot 3) beam_bot_four beam_bot_three (ids_bot 3) rocker_bot state player
	
	
	
	
	
    
