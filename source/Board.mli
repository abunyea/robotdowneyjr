type space = P1 | P2 | Grey | Empty | Void
type board = space array array
type move = int * int * int * int
type move_set = move list

val print_board : board -> unit
val read_input : unit -> board
val copy_board : board -> board
val do_move : board -> move -> board

(* Prints a move for std out, with appropriate
   -1s to not place a gray marble. *)
val string_of_move : move -> string
