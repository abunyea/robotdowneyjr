open Board

type state = { player : space;
               status : int;
               time1 : int;
               time2 : int;
               board : board;
               grey_remain_1 : int;
               grey_remain_2 : int; }

(* Read the whole board, plus all other state
   related info. The player is who goes first *)
val read_initial_input : unit -> space * state

(* Read a single line containing state
   info, and construct a new state using
   the previous state *)
val read_opponent_input : state -> state

(* Do move on the board of this state *)
val update_board : state -> move -> state
