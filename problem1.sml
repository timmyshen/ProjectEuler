(* Problem 1: Multiples of 3 and 5*)

(* SML *)

fun multiples_of_x_below_n x n =
    if n <= x then []
    (* else if n = x then [x] *)
    else (* n > x *)
	if (n - 1) mod x = 0 then
	    ((n - 1) :: (multiples_of_x_below_n x (n - 1)))
	else multiples_of_x_below_n x (n - 1)

fun sum_int_list ls =
    List.foldl (fn (x, y) => x + y) 0 ls

val list3_1000 = multiples_of_x_below_n 3 1000
val list5_1000 = multiples_of_x_below_n 5 1000
val list15_1000 = multiples_of_x_below_n 15 1000

val answer = (sum_int_list list3_1000) + 
	     (sum_int_list list5_1000) -
	     (sum_int_list list15_1000)

fun multiples_of_xs x = List.filter (fn l => (l mod x) = 0)

fun mod_or x denoms = 
    let
	val tfList = List.map (fn y => (x mod y) = 0) denoms 
    in
	List.exists (fn s => s = true) tfList
    end

fun mod_or2 x denoms =
    case denoms of
	[] => false
      | hd::tl => 
	if x mod hd = 0 then true
	else mod_or2 x tl
