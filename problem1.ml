(* Problem 1: Multiples of 3 and 5 *)
(* If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
 *)

(* OCaml *)
open Core.Std

let rec multiples_of_x_below_n x n =
  if n < x then []
  else if n = x then [x]
  else (* n > x *)
    if n mod x = 0 then (n :: (multiples_of_x_below_n x (n - 1)))
    else multiples_of_x_below_n x (n - 1)

let rec print_int_list ls = function
  [] -> ()
  | hd :: tl -> print_int hd; print_string " "; print_int_list tl

let () =
  let ls3 = multiples_of_x_below_n 3 10 in
  let ls5 = multiples_of_x_below_n 5 10 in
  print_int_list ls3 @ ls5
