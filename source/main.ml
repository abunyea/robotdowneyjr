open Board;;

let brd = read_input() in
let brd1 = do_move brd (9, 3, 8, 4) in

((print_board brd);
(print_newline());
(print_board brd1));;
