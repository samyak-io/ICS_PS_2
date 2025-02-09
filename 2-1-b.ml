let rec gcd (a:int) (b:int) : int = 
  match (a,b) with
  | (0, _) -> b
  | (_, 0) -> a
  | _ -> gcd b (a mod b)
;;
print_int(gcd 12 8);;