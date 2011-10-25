open Board
open State
open Manhattan_Bot

let middle = ref false
let ending = ref false

(* checks distance between furthest marbles on each side,
 * updates middle *)
let check_middle state : unit = failwith "unimplemented"

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
