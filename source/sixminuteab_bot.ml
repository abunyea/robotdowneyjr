open Board
open State
open JohnRocker_bot
open Baby_bot
open Minimax_bot
open Manhattan_Bot

(* minimum distance before switching to minimax *)
let middle_distance = 8
(* Number of their marbles that must have passed all of ours
   before we switch to ending bot*)
let num_ending = 8

(* Criteria for switching to john rocker bot:*)
(*  all pieces in last 6 rows. At least 5 in last 4 *)

let build_piece_list_a board player = 
	build_piece_list board player 0 0 [];;

let middle = ref false
let ending = ref false
let closer = ref false

(* checks distance between furthest marbles on each side,
 * updates middle *)
let check_middle state : unit = 
  if not !middle then 
    let p1_list = build_piece_list_a state.board P1 in
    let p2_list = build_piece_list_a state.board P2 in
    let furthest_p1 = List.fold_left (fun a (_, x) -> min a x) 19 p1_list in
    let furthest_p2 = List.fold_left (fun a (_, x) -> max a x) (-1) p2_list in
    middle := abs (furthest_p1 - furthest_p2) <= middle_distance

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
    ending := num_behind >= num_ending

let check_closer state player : unit =
	if not !closer then
	let our_pieces = build_piece_list state.board player 0 0 [] in
	let y_dist = List.map (fun (x, y) -> if player = P1 then y else 16 - y) our_pieces in
	let (num_less_four, num_less_six) = List.fold_left (fun (less_four, less_six) y_dist -> if y_dist < 4 then (less_four + 1, less_six + 1) else (if y_dist < 6 then (less_four, less_six + 1) else (less_four, less_six))) (0, 0) y_dist in
	if (num_less_four >= 6 && num_less_six = 10) then (
		prerr_endline "Bringing in the lefty.."; closer:= true) else ()
	
let sixminuteab_bot state player =
  check_middle state;
  if not !middle then
	(* Starter *)
    baby_bot state player

  else (check_ending state;
  if not !ending then
    (* Middle Relief *) 
			basic_alphabeta_bot state player

  else (check_closer state player;
	if not !closer then
		(* Set up man *)
			baby_bot state player
		else 
		(* Closer *)
			rocker_bot state player))
			
	
    
