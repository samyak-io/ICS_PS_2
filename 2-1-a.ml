type nat = Zero | Succ of nat;;

let rec minus (n:nat) (m:nat) : nat =
  match (n,m) with 
  | (Zero, _) -> Zero (*not allowing negatives*)
  | (n, Zero) -> n 
  | (Succ n', Succ m') -> minus n' m'
;;


type comparison = Less | Equal | Greater;;

let rec compare_nat (a: nat) (b:nat) : comparison = 
  match (a,b) with
  | (Zero, Zero) -> Equal
  | (_, Zero) -> Greater
  | (Zero, _) -> Less
  | (Succ a', Succ b') -> compare_nat a' b'
;;


let rec gcd (a:nat) (b:nat) : nat =
  if a = Zero then b
  else if b = Zero then a
  else
    match compare_nat a b with
    | Equal -> a
    | Greater -> gcd (minus a b) b
    | Less -> gcd a (minus b a)
;;

let rec nat_to_int (n: nat) : int =
  match n with
  | Zero -> 0
  | Succ n' -> 1 + nat_to_int n';;

(*Trying without nat, using regular int*)
let rec gcd_int (a: int) (b:int) : int =
  match (a,b) with
  | (0,_) -> b
  | (_,0) -> a
  | (a,b) when a > b -> gcd_int (a-b) b
  | _ -> gcd_int a (b-a)
;;

