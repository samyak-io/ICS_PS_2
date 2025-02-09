let rec extended_gcd a b =
  match b with
  | 0 -> (a, 1, 0)  (* Base case: gcd(a, 0) = a, x = 1, y = 0 *)
  | _ ->
      let (g, x', y') = extended_gcd b (a mod b) in
      (g, y', x' - (a / b) * y')  (* Backtracking step *)
;;
(* Example usage *)
let (g, x, y) = extended_gcd 30 20;;
Printf.printf "gcd: %d, x: %d, y: %d\n" g x y;;
