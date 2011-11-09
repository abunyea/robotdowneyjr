open Board
open State

(* Returns a move based on one heuristic: Make the move such that the*)
(*collective manhattan distance of all of our pieces from the opposite point*)
(*of the board is minimized*)

val manhattan_bot : state -> space -> move