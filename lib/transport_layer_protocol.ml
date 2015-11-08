
open Printf

type t =
| UDP 
| TCP
| ICMP
with compare

let to_string to_string_mode t =
  match t with
  | UDP -> "UDP"
  | TCP -> "TCP"
  | ICMP -> "ICMP"

(* let to_int t =  *)
(*   match t with *)
(*   | UDP -> 0 *)
(*   | TCP -> 1 *)
(*   | ICMP -> 2 *)

let equal t1 t2 = Batteries.Int.equal 0 (compare t1 t2)
