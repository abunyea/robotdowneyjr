open Board

type move = (int * int * int * int)
type move_set = move list

let string_of_move (x1, y1, x2, y2) =
  (string_of_int x1) ^ " " ^ (string_of_int y1) ^ " " ^
  (string_of_int x2) ^ " " ^ (string_of_int y2) ^ " -1 -1"

let available_moves brd = 
  let stub = [(8,8,8,8)] in
stub