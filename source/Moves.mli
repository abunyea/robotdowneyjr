open Board

type move
type move_set
val available_moves : board -> move_set
(* Prints a move for std out, with appropriate
   -1s to not place a gray marble. *)
val string_of_move : move -> string