
open Printf
  
open Ip_address_data_structures_instantiations

let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Prefix_utils]: %s@." s)
      else
        ignore
    )
    fmt

let bits t =
  match t with
  | Ipaddr.V4 prefixv4 -> Ipaddr.V4.Prefix.bits prefixv4
  | Ipaddr.V6 prefixv6 -> Ipaddr.V6.Prefix.bits prefixv6

let prefix_distance t1 t2 =
  match t1,t2 with
  | Ipaddr.V4 a, Ipaddr.V4 b -> Ip_address_data_structures_V4.Prefix_utils.prefix_distance a b
  | Ipaddr.V6 a, Ipaddr.V6 b -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv6 not implemented")
  | Ipaddr.V4 _, Ipaddr.V6 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv4 and IPv6")
  | Ipaddr.V6 _, Ipaddr.V4 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv6 and IPv4") 

let common_prefix_betwen_prefix t1 t2 =
  match t1,t2 with
  | Ipaddr.V4 a, Ipaddr.V4 b -> Ipaddr.V4 (Ip_address_data_structures_V4.Prefix_utils.common_prefix_betwen_prefix a b)
  | Ipaddr.V6 a, Ipaddr.V6 b -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv6 not implemented")
  | Ipaddr.V4 _, Ipaddr.V6 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv4 and IPv6")
  | Ipaddr.V6 _, Ipaddr.V4 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv6 and IPv4") 

let prefix_included_2_in_1 t1 t2 =
  match t1,t2 with
  | Ipaddr.V4 a, Ipaddr.V4 b -> Ip_address_data_structures_V4.Prefix_utils.prefix_included_2_in_1 a b
  | Ipaddr.V6 a, Ipaddr.V6 b -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv6 not implemented")
  | Ipaddr.V4 _, Ipaddr.V6 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv4 and IPv6")
  | Ipaddr.V6 _, Ipaddr.V4 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv6 and IPv4") 

let prefix_included_12 t1 t2 =
  match t1,t2 with
  | Ipaddr.V4 a, Ipaddr.V4 b -> Ip_address_data_structures_V4.Prefix_utils.prefix_included_12 a b
  | Ipaddr.V6 a, Ipaddr.V6 b -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv6 not implemented")
  | Ipaddr.V4 _, Ipaddr.V6 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv4 and IPv6")
  | Ipaddr.V6 _, Ipaddr.V4 _ -> raise (Invalid_argument "Anomaly_ip_address_compact: inter: IPv6 and IPv4") 
