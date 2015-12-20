
module L = BatList
module A = BatArray
  
open Ip_address_data_structures_instantiations

type t =
  | Empty
  | V4 of Ip_address_data_structures_V4.Sequence.t
  | V6 of Ip_address_data_structures_V6.Sequence.t
with compare, sexp, bin_io

let to_string t =
  match t with
  | Empty -> ""
  | V4 sequence -> Ip_address_data_structures_V4.Sequence.to_string sequence
  | V6 sequence -> Ip_address_data_structures_V6.Sequence.to_string sequence
  
let of_ipaddr_array (a : Ipaddr.t array) =
  match A.length a with
  | 0 -> Empty
  | _ ->
    let first_ipaddr = a.(0) in

    let is_v4 =
      match first_ipaddr with
      | Ipaddr.V4 _ -> true
      | Ipaddr.V6 _ -> false
    in

    if is_v4 then
      let v4_a =
        A.map
          (fun ipaddr ->
             match ipaddr with
             | Ipaddr.V4 ipaddr -> ipaddr
             | Ipaddr.V6 _ -> raise (Invalid_argument "Ip_address_sequence: of_ipaddr_array: IPv6 when expected IPv4")
          )
          a
      in

      V4
        (Ip_address_data_structures_V4.Sequence.of_ipaddr_array v4_a)
    else
      let v6_a =
        A.map
          (fun ipaddr ->
             match ipaddr with
             | Ipaddr.V4 _ -> raise (Invalid_argument "Ip_address_sequence: of_ipaddr_array: IPv4 when expected IPv6")
             | Ipaddr.V6 ipaddr -> ipaddr
          )
          a
      in

      V6 (Ip_address_data_structures_V6.Sequence.of_ipaddr_array v6_a)
  
let to_ipaddr_array t =
  match t with
  | Empty -> raise (Invalid_argument "[Ip_address_container]: to_ipaddr_list: Empty")
  | V4 sequence ->
    A.map
      (fun v4 -> Ipaddr.V4 v4)
      (Ip_address_data_structures_V4.Sequence.to_ipaddr_array sequence)
  | V6 sequence ->
    A.map
      (fun v6 -> Ipaddr.V6 v6)
      (Ip_address_data_structures_V6.Sequence.to_ipaddr_array sequence)
  
let length t =
  match t with
  | Empty -> raise (Invalid_argument "[Ip_address_container]: length: Empty")
  | V4 sequence -> Ip_address_data_structures_V4.Sequence.length sequence
  | V6 sequence -> Ip_address_data_structures_V6.Sequence.length sequence


