
module L = BatList
  
open Ip_address_data_structures_instantiations

type t =
  | Empty
  | V4 of Ip_address_data_structures_V4.Container.t
  | V6 of Ip_address_data_structures_V6.Container.t
with compare
(* with compare, sexp, bin_io *)

let to_string t =
  match t with
  | Empty -> ""
  | V4 container -> Ip_address_data_structures_V4.Container.to_string container
  | V6 container -> Ip_address_data_structures_V6.Container.to_string container     
  
let of_ipaddr_list ipaddr_list =
  assert(L.length ipaddr_list > 0);

  (* let first_ipaddr = L.hd ipaddr_list in *)

  (* let is_v4 = *)
  (*   match first_ipaddr with *)
  (*   | Ipaddr.V4 -> true *)
  (*   | Ipaddr.V6 -> false  *)
  (* in *)

  let l =
    L.map
      (fun ipaddr ->
         Uint32.of_int32
     (Ipaddr.V4.to_int32
              (* (Ipaddr.to_v4 (fst tuple)) *)
              (match ipaddr with
               | Ipaddr.V4 ipaddr -> ipaddr
               | Ipaddr.V6 _ -> raise (Invalid_argument "[Ip_address_container]: of_detailed_metrics: IPv6 when expected IPv4")
              )
           )
      )
      ipaddr_list
  in

  V4 (Ip_address_data_structures_V4.Container.of_list l)
        
  (* if is_v4 then *)
  (*   ( *)
  (*   ) *)
  (* else *)
  (*   (   *)

  (*   ) *)
  
let to_ipaddr_list t =
  match t with
  | Empty -> raise (Invalid_argument "[Ip_address_container]: to_ipaddr_list: Empty")
  | V4 container -> Ip_address_data_structures_V4.Container.to_ipaddr_list container
  | V6 container -> Ip_address_data_structures_V6.Container.to_ipaddr_list container     
  
let length t =
  match t with
  | Empty -> raise (Invalid_argument "[Ip_address_container]: length: Empty")
  | V4 container -> Ip_address_data_structures_V4.Container.length container
  | V6 container -> Ip_address_data_structures_V6.Container.length container

let prefix t =
  match t with
  | Empty -> raise (Invalid_argument "[Ip_address_container]: prefix: Empty")
  | V4 container ->
    Ipaddr.V4
      (Ip_address_data_structures_V4.Container.prefix
         container
      )
  | V6 container ->
    Ipaddr.V6
      (Ip_address_data_structures_V6.Container.prefix
         container
      )



let of_ip_address_container_compact container_compact =
  match container_compact with
  | Ip_address_container_compact.Empty -> raise (Invalid_argument "[Ip_address_container]: of_ip_address_container_compact: Empty")
  | Ip_address_container_compact.V4 container_compact_V4 ->
    V4
      (Ip_address_data_structures_V4.Container.of_ip_address_container_compact
         container_compact_V4
      )
  | Ip_address_container_compact.V6 container_compact_V6 ->
    V6
      (Ip_address_data_structures_V6.Container.of_ip_address_container_compact
         container_compact_V6
      )

let to_ip_address_container_compact t =
  match t with
  | Empty -> raise (Invalid_argument "[Ip_address_container]: to_ip_address_container_compact: Empty")
  | V4 container ->
    Ip_address_container_compact.V4
      (Ip_address_data_structures_V4.Container.to_ip_address_container_compact
         container
      )
  | V6 container ->
    Ip_address_container_compact.V6
      (Ip_address_data_structures_V6.Container.to_ip_address_container_compact
         container
      )




let inter t1 t2 =
  match t1,t2 with
  | Empty, Empty -> Empty
  | V4 a, V4 b -> V4 (Ip_address_data_structures_V4.Container.inter a b)
  | V6 a, V6 b -> V6 (Ip_address_data_structures_V6.Container.inter a b)
  | Empty, V4 a -> V4 a
  | V4 a, Empty -> V4 a
  | Empty, V6 a -> V6 a
  | V6 a, Empty -> V6 a
  | V4 _, V6 _ -> raise (Invalid_argument "[Ip_address_container]: inter: IPv4 and IPv6")
  | V6 _, V4 _ -> raise (Invalid_argument "[Ip_address_container]: inter: IPv6 and IPv4")

let union t1 t2 =
  match t1,t2 with
  | Empty, Empty -> Empty
  | V4 a, V4 b -> V4 (Ip_address_data_structures_V4.Container.union a b)
  | V6 a, V6 b -> V6 (Ip_address_data_structures_V6.Container.union a b)
  | Empty, V4 a -> V4 a
  | V4 a, Empty -> V4 a
  | Empty, V6 a -> V6 a
  | V6 a, Empty -> V6 a
  | V4 _, V6 _ -> raise (Invalid_argument "[Ip_address_container]: union: IPv4 and IPv6")
  | V6 _, V4 _ -> raise (Invalid_argument "[Ip_address_container]: union: IPv6 and IPv4")


let get_consecutiveness_empty_run_length_std_mean_med_centiles_max t =
  match t with
  | Empty -> raise (Invalid_argument "[Ip_address_container:] get_consecutiveness_empty_run_length_std_mean_med_centiles_max: Empty")
  | V4 container -> Ip_address_data_structures_V4.Container.get_consecutiveness_empty_run_length_std_mean_med_centiles_max container
  | V6 container -> Ip_address_data_structures_V6.Container.get_consecutiveness_empty_run_length_std_mean_med_centiles_max container

let consecutive_number t =
  match t with
  | Empty -> raise (Invalid_argument "[Ip_address_container:] consecutive_number: Empty")
  | V4 container -> Ip_address_data_structures_V4.Container.consecutive_number container
  | V6 container -> Ip_address_data_structures_V6.Container.consecutive_number container

let jaccard t1 t2 =
  match t1,t2 with
  | Empty, Empty -> 0.
  | V4 a, V4 b -> Ip_address_data_structures_V4.Container.jaccard a b
  | V6 a, V6 b -> Ip_address_data_structures_V6.Container.jaccard a b
  | Empty, V4 a -> 0.
  | V4 a, Empty -> 0.
  | Empty, V6 a -> 0.
  | V6 a, Empty -> 0.
  | V4 _, V6 _ -> raise (Invalid_argument "[Ip_address_container]: union: IPv4 and IPv6")
  | V6 _, V4 _ -> raise (Invalid_argument "[Ip_address_container]: union: IPv6 and IPv4")

  (* | V4 a, V4 b -> Ip_address_data_structures_V4.Container.jaccard a b *)
  (* | V6 a, V6 b -> Ip_address_data_structures_V6.Container.jaccard a b *)


let fold_right_ipaddr f t init =
  match t with
  | Empty -> init
  | V4 container -> Ip_address_data_structures_V4.Container.fold_right_ipaddr f container init
  | V6 container -> Ip_address_data_structures_V6.Container.fold_right_ipaddr f container init
    
let number_24_prefix t =
  match t with
  | Empty -> 0
  | V4 container -> Ip_address_data_structures_V4.Container.number_24_prefix container
  | V6 container -> Ip_address_data_structures_V6.Container.number_24_prefix container

let remove_borders border_percentage t =
  match t with
  | Empty -> Empty
  | V4 container -> V4 (Ip_address_data_structures_V4.Container.remove_borders border_percentage container)
  | V6 container -> V6 (Ip_address_data_structures_V6.Container.remove_borders border_percentage container)


let coverage_min_max t =
  match t with
  | Empty -> 0.
  | V4 container -> Ip_address_data_structures_V4.Container.coverage_min_max container
  | V6 container -> Ip_address_data_structures_V6.Container.coverage_min_max container

let coverage_min_max_prefix prefix t =
  match t with
  | Empty -> 0.
  | V4 container ->
    let prefix_v4 =
      match prefix with
      | Ipaddr.V4 a -> a
      | Ipaddr.V6 a -> raise (Invalid_argument "[Ip_address_container]: coverage_in_prefix: IPv6 prefix provided to IPv4 container")
    in
    Ip_address_data_structures_V4.Container.coverage_min_max_prefix prefix_v4 container
  | V6 container ->
    let prefix_v6 =
      match prefix with
      | Ipaddr.V4 a -> raise (Invalid_argument "[Ip_address_container]: coverage_in_prefix: IPv4 prefix provided to IPv6 container")
      | Ipaddr.V6 a -> a
    in
    Ip_address_data_structures_V6.Container.coverage_min_max_prefix prefix_v6 container

let coverage_prefix prefix t =
  match t with
  | Empty -> 0.
  | V4 container ->
    let prefix_v4 =
      match prefix with
      | Ipaddr.V4 a -> a
      | Ipaddr.V6 a -> raise (Invalid_argument "[Ip_address_container]: coverage_in_prefix: IPv6 prefix provided to IPv4 container")
    in
    Ip_address_data_structures_V4.Container.coverage_prefix prefix_v4 container
  | V6 container ->
    let prefix_v6 =
      match prefix with
      | Ipaddr.V4 a -> raise (Invalid_argument "[Ip_address_container]: coverage_in_prefix: IPv4 prefix provided to IPv6 container")
      | Ipaddr.V6 a -> a
    in
    Ip_address_data_structures_V6.Container.coverage_prefix prefix_v6 container


let extract_smallest_biggest_part_around_biggest_gap t =
  match t with
  | Empty -> (Empty, Empty)
  | V4 container ->
    let tuple = Ip_address_data_structures_V4.Container.extract_smallest_biggest_part_around_biggest_gap container in
    V4 (fst tuple), V4 (snd tuple)
  | V6 container ->
    let tuple = Ip_address_data_structures_V6.Container.extract_smallest_biggest_part_around_biggest_gap container in
    V6 (fst tuple), V6 (snd tuple)
