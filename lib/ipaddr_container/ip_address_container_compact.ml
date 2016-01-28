
module L = BatList
  
open Ip_address_data_structures_instantiations

type t =
  | Empty
  | V4 of Ip_address_data_structures_V4.Container_compact.t
  | V6 of Ip_address_data_structures_V6.Container_compact.t
with compare, sexp, bin_io

(* let of_ipaddr_list ipaddr_list = *)
(*   assert(L.length ipaddr_list > 0); *)

(*   let l = *)
(*     L.map *)
(*       (fun ipaddr -> *)
(*    Ipaddr.V4.to_int32 *)
(*            (\* (Ipaddr.to_v4 (fst tuple)) *\) *)
(*            (match ipaddr with *)
(*             | Ipaddr.V4 ipaddr -> ipaddr *)
(*             | Ipaddr.V6 _ -> raise (Invalid_argument "Anomaly_ip_address: of_detailed_metrics: IPv6 when expected IPv4") *)
(*            ) *)
(*       ) *)
(*       ipaddr_list *)
(*   in *)

(*   V4 (Ip_address_data_structures_V4.Container_compact.of_list l) *)


let of_ipaddr_list ipaddr_list =
  (* assert(L.length ipaddr_list > 0); *)

  match L.length ipaddr_list with
  | 0 -> Empty
  | _ ->
    let first_ipaddr = L.hd ipaddr_list in

    let is_v4 =
      match first_ipaddr with
      | Ipaddr.V4 _ -> true
      | Ipaddr.V6 _ -> false
    in

    if is_v4 then
      let l =
        L.map
          (fun ipaddr ->
             Ipaddr.V4.to_int32
               (* (Ipaddr.to_v4 (fst tuple)) *)
               (match ipaddr with
                | Ipaddr.V4 ipaddr -> ipaddr
                | Ipaddr.V6 _ -> raise (Invalid_argument "Anomaly_ip_address: of_detailed_metrics: IPv6 when expected IPv4")
               )
          )
          ipaddr_list
      in

      V4 (Ip_address_data_structures_V4.Container_compact.of_list l)
    else
      let int32_tuple_l =
        L.map
          (fun ipaddr ->
             Ipaddr.V6.to_int32
               (match ipaddr with
                | Ipaddr.V4 ipaddr -> raise (Invalid_argument "Anomaly_ip_address: of_detailed_metrics: IPv4 when expected IPv6")
                | Ipaddr.V6 ipaddr -> ipaddr
               )
          )
          ipaddr_list
      in

      let l =
        L.map
          (fun (a, b, c, d) ->
             Int64.logor (Int64.shift_left (Int64.of_int32 a) 32) (Int64.of_int32 b),
             Int64.logor (Int64.shift_left (Int64.of_int32 c) 32) (Int64.of_int32 d)
          )
          int32_tuple_l
      in

      V6 (Ip_address_data_structures_V6.Container_compact.of_list l)

      (* if is_v4 then *)
      (*   ( *)
      (*   ) *)
      (* else *)
      (*   (   *)

      (*   ) *)

let length t =
  match t with
  | Empty -> 0
  | V4 container_compact -> Ip_address_data_structures_V4.Container_compact.length container_compact
  | V6 container_compact -> Ip_address_data_structures_V6.Container_compact.length container_compact

let to_string t =
  match t with
  | Empty -> "Empty"
  | V4 container_compact -> Ip_address_data_structures_V4.Container_compact.to_string container_compact
  | V6 container_compact -> Ip_address_data_structures_V6.Container_compact.to_string container_compact

  
let consecutive t =
  match t with
  | Empty -> Empty
  | V4 container -> V4 (Ip_address_data_structures_V4.Container_compact.consecutive container)
  | V6 container -> V6 (Ip_address_data_structures_V6.Container_compact.consecutive container)




let inter t1 t2 =
  match t1, t2 with
  | Empty, Empty -> Empty
  | V4 a, V4 b -> V4 (Ip_address_data_structures_V4.Container_compact.inter a b)
  | V6 a, V6 b -> V6 (Ip_address_data_structures_V6.Container_compact.inter a b)
  | Empty, V4 a -> V4 a
  | V4 a, Empty -> V4 a
  | Empty, V6 a -> V6 a
  | V6 a, Empty -> V6 a
  | V4 _, V6 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv4 and IPv6")
  | V6 _, V4 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv6 and IPv4")

let union t1 t2 =
  match t1,t2 with
  | Empty, Empty -> Empty
  | V4 a, V4 b -> V4 (Ip_address_data_structures_V4.Container_compact.union a b)
  | V6 a, V6 b -> V6 (Ip_address_data_structures_V6.Container_compact.union a b)
  | Empty, V4 a -> V4 a
  | V4 a, Empty -> V4 a
  | Empty, V6 a -> V6 a
  | V6 a, Empty -> V6 a
  | V4 _, V6 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv4 and IPv6")
  | V6 _, V4 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv6 and IPv4")

