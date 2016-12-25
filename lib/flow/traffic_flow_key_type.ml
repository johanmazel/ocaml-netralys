
open Printf

type simple_t = Five_tuple_flow.t
let to_string_simple simple_t = Five_tuple_flow.to_string simple_t
let to_simple_filename_prefix simple_t = Five_tuple_flow.to_filename_prefix simple_t
let compare_simple = Five_tuple_flow.compare

type aggr_mode_t =
  | Src_addr_mode
  | Dst_addr_mode
  | All_mode
    
let to_string_aggr_mode  t =
  match t with
  | Src_addr_mode -> "src_addr_mode"
  | Dst_addr_mode -> "dst_addr_mode"
  | All_mode -> "all_mode"

type aggr_t =
  | Src_addr of Ipaddr.t
  | Dst_addr of Ipaddr.t
  | All
[@@deriving compare]

let to_string_aggr t = 
  match t with
  | Src_addr src_addr -> "src_" ^ (Ipaddr.to_string src_addr)
  | Dst_addr dst_addr -> "dst_" ^ (Ipaddr.to_string dst_addr)
  | All -> "global"

let to_aggr_filename_prefix aggr_t =
  to_string_aggr aggr_t

let of_aggr_mode_simple_key
    aggr_mode
    simple_key
  =
  match aggr_mode with
  | Src_addr_mode ->
    Src_addr
      (* (Ipaddr.V4.to_int32 (Ipaddr.to_v4 simple_key.Five_tuple_flow.src_addr)) *)
      simple_key.Five_tuple_flow.src_addr
  | Dst_addr_mode ->
    Dst_addr
      (* (Ipaddr.V4.to_int32 (Ipaddr.to_v4 simple_key.Five_tuple_flow.dst_addr)) *)
      simple_key.Five_tuple_flow.dst_addr
  | All_mode -> All

let of_aggr_mode_aggr_key 
    aggr_mode
    aggr_key
    =
  match aggr_key with
  | Src_addr src_addr ->
    (
      match aggr_mode with
      | Src_addr_mode -> failwith "Time_aggregation_detailed: trying to aggregate from src_addr to src_addr => useless"
      | Dst_addr_mode -> failwith "Time_aggregation_detailed: trying to aggregate from src_addr to dst_addr => impossible"
      | All_mode -> All
    )
  | Dst_addr dst_addr ->
    (
      match aggr_mode with
      | Src_addr_mode -> failwith "Time_aggregation_detailed: trying to aggregate from dst_addr to dst_addr => impossible"
      | Dst_addr_mode -> failwith "Time_aggregation_detailed: trying to aggregate from dst_addr to src_addr => useless"
      | All_mode -> All
    )
  | All ->
    (
      match aggr_mode with
      | Src_addr_mode -> failwith "Time_aggregation_detailed: trying to aggregate from src_addr to all => not possible"
      | Dst_addr_mode -> failwith "Time_aggregation_detailed: trying to aggregate from src_addr to all => not possible"
      | All_mode -> failwith "Time_aggregation_detailed: trying to aggregate from all to all => useless"
    )

let compare_aggr = compare
