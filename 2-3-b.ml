(*
   The Ackermann function is a special function in mathematics that grows very fast.
   It is used in computer science to test recursion and understand how deep function
   calls can go.

   It is defined as:
   - A(0, n) = n + 1
   - A(m, 0) = A(m - 1, 1), when m > 0
   - A(m, n) = A(m - 1, A(m, n - 1)), when m > 0 and n > 0

   This function is **not practical** for large inputs, because it grows extremely fast!
   Example:
   - A(1, 2) = 4
   - A(2, 3) = 9
   - A(3, 2) = 29
   - A(4, 1) is already too large for most computers to handle!

   This implementation follows the recursive definition of the Ackermann function.
*)

let rec ackermann (m: int) (n: int) : int =
  match (m, n) with
  | (0, n) -> n + 1
  | (m, 0) when m > 0 -> ackermann (m - 1) 1
  | (m, n) when m > 0 && n > 0 -> ackermann (m - 1) (ackermann m (n - 1))
;;
let () = 
  Printf.printf "Ackermann(2, 3) = %d\n" (ackermann 4 1)