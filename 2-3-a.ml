(* 
   Fast exponentiation using recursion. 
   Instead of multiplying x repeatedly, we use the idea that: 
   - If n is 0, x^n = 1.
   - If n is even, x^n = (x^2)^(n/2), reducing the number of multiplications.
   - If n is odd, x^n = x * (x^2)^((n-1)/2), applying the even case after one multiplication.
   
   This method is much faster than simple repeated multiplication.
   Example: 2^10 computes in just 4 steps instead of 10!
*)

let rec f_exp (x: int) (n: int) : int = 
  match (x, n) with
  | (_, 0) -> 1 
  | (0, _) -> 0  
  | (_, n) when n mod 2 = 0 ->
      let result = f_exp x (n / 2) in
      result * result
  | _ -> x * f_exp x (n - 1)
;;

let () = 
  print_int(f_exp 2 10)

