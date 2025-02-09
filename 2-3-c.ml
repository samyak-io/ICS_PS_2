(*
   The Collatz Conjecture is a simple but unsolved problem in mathematics.
   It states that if you start with any positive number and follow these rules,
   you will always reach 1:

   - If the number is even, divide it by 2.
   - If the number is odd, multiply it by 3 and add 1.
   - Repeat this process until you get 1.

   This program counts how many steps it takes to reach 1.

   Example:
   - Starting with 5: 5 → 16 → 8 → 4 → 2 → 1 (Takes 5 steps)
   - Starting with 12: 12 → 6 → 3 → 10 → 5 → 16 → ... → 1 (Takes 9 steps)

   Mathematicians believe this works for all numbers, but no one has proved it yet!
*)

let rec collatz x count =
  match x with 
  | 1 -> count
  | _ when x mod 2 = 0 -> collatz (x / 2) (count + 1)
  | _ -> collatz (3 * x + 1) (count + 1)
;;

let collatz_count x = collatz x 0;;

let () = 
  let x = 5 in
  Printf.printf "Number of steps for %d: %d\n" x (collatz_count x)
;;