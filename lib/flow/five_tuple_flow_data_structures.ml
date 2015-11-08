
open Printf

module A = BatArray

module Five_tuple_flow_hashset = struct
  include Hashset.Make(Five_tuple_flow)

  (* type t = Five_tuple_flow.t Hashset.t *)

  (* let of_list l =  *)
  (*   let hs = Hashset.create 0 in *)
  (*   L.iter (fun x -> Hashset.add hs x) l; *)
  (*   hs *)
  (* let to_list = elements *)

  let empty = empty ()
  let cardinal = length
  let elements t = A.to_list (keys t)
  let singleton key = of_list [ key ]

  (* let to_string *)
  (*     ?sep: (sep = " ") *)
  (*     t *)
  (*     = *)
  (*   let list = elements t in *)
    
  (*   List_ext.to_string *)
  (*     ~sep *)
  (*     Five_tuple_flow.to_string *)
  (*     list *)

end

(* include Core.Comparable.Make(Five_tuple_flow) *)
(* include Core.Hashable.Make(Five_tuple_flow) *)
