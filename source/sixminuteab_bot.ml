open Board
open State
open Manhattan_Bot

(* minimum distance before switching to minimax *)
let middle_distance = 8

let middle = ref false
let ending = ref false

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
let check_ending state : unit = failwith "ahhhhh"

let sixminuteab_bot state player =
  check_middle state;
  if not !middle then
	(* manhattan *)
    manhattan_bot state player

  else (check_ending state;
  if not !ending then
    (* minimax *) failwith "no"

  else
    (* lookup table or search *) failwith "no")
