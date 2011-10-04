type space = P1 | P2 | Grey | Empty | Void
type board = space array array
type move = int * int * int * int
type move_set = move list
type player = Player1 | Player2

val print_board : board -> unit
val copy_board : board -> board
val do_move : board -> move -> board
val char_to_space : char -> space
val player_of_int : int -> player
val toggle_player : player -> player

val get_p1_home : board -> space list
val get_p2_home : board -> space list
val has_won : board -> player -> bool

(* Prints a move for std out, with appropriate
   -1s to not place a gray marble. *)
val string_of_move : move -> string

(*Generates a list of available moves in the int*int*int*int format*)
(* Takes as input a board and space, which must be either P1 or P2*)
val available_moves : board -> space -> move_set

val print_movelist : move list -> unit
