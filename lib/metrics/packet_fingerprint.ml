
open Printf

open Sexplib.Std

type t =
  | None
  | Zmap
  | Masscan
  | Zmap_masscan
with compare, sexp, bin_io

let to_int t =
  match t with
  | None -> 0
  | Zmap -> 1
  | Masscan -> 2
  | Zmap_masscan -> 3
      
let to_string t =
  match t with
  | None -> "none"
  | Zmap -> "zmap"
  | Masscan -> "masscan"
  | Zmap_masscan -> "zmap_masscan"
    
let of_packet_data_for_metrics
    packet_data_for_metrics
  =
  (
    match packet_data_for_metrics.Packet_data_for_metrics.pdu_t with
    | Packet_data_for_metrics.IPV4 ipv4_data_for_metrics ->
      (
        let ip_id, dst_addr =
          ipv4_data_for_metrics.Ipv4_data_for_metrics.ip_id
          ,
          Ipaddr.V4.to_int32 ipv4_data_for_metrics.Ipv4_data_for_metrics.destination_address
        in

        let default_dst_port_seq_num_tuple = Int32.zero, Int32.zero in
        let dst_port, seq_number =
          match ipv4_data_for_metrics.Ipv4_data_for_metrics.pdu_t with
          | Ipv4_data_for_metrics.ICMP icmp_data_for_metrics ->
            default_dst_port_seq_num_tuple
          | Ipv4_data_for_metrics.TCP tcp_data_for_metrics ->
            (Int32.of_int tcp_data_for_metrics.Tcp_data_for_metrics.destination_port)
            ,
            (Int32.of_int tcp_data_for_metrics.Tcp_data_for_metrics.seq_number)
          | Ipv4_data_for_metrics.UDP udp_data_for_metrics ->
            (Int32.of_int udp_data_for_metrics.Udp_data_for_metrics.destination_port)
            ,
            Int32.zero    
          | Ipv4_data_for_metrics.IPv6 ipv6_data_for_metrics ->
            default_dst_port_seq_num_tuple
          | Ipv4_data_for_metrics.GRE ->
            default_dst_port_seq_num_tuple
          | Ipv4_data_for_metrics.Other _ ->
            default_dst_port_seq_num_tuple

        in

        (* print_endline *)
        (*   (sprintf *)
        (*      "Packet_fingerprint: of_packet_data_for_metrics: ip_id: %d ; dst_addr: %ld (%s)\ndst_port: %ld ; seq_number : %ld" *)
        (*      ip_id *)
        (*      dst_addr *)
        (*      (Ipaddr.V4.to_string ipv4_data_for_metrics.Ipv4_data_for_metrics.destination_address) *)
        (*      dst_port *)
        (*      seq_number *)
        (*   ); *)

        let is_zmap =
          match ip_id with
          | 54321 -> true
          | _ -> false
        in

        (* print_endline *)
        (*   (sprintf *)
        (*      "Packet_fingerprint: of_packet_data_for_metrics: ip_id: %d ?= %ld (%ld)" *)
        (*      ip_id *)
        (*      ( *)
        (*        Int32.logand  *)
        (*          0xFFFF_l *)
        (*          ( *)
        (*            Int32.logxor *)
        (*              dst_addr *)
        (*              ( *)
        (*                Int32.logxor *)
        (*                  dst_port *)
        (*                  seq_number *)
        (*              ) *)
        (*          ) *)
        (*      ) *)
        (*      ( *)
        (*        Int32.logxor *)
        (*          dst_addr *)
        (*          ( *)
        (*            Int32.logxor *)
        (*              dst_port *)
        (*              seq_number *)
        (*          ) *)
        (*      ) *)
        (*   ); *)

        let is_masscan =
          (Int32.of_int ip_id)
          =
          Int32.logand 
            0xFFFF_l
            (
              Int32.logxor
                dst_addr
                (
                  Int32.logxor
                    dst_port
                    seq_number
                )
            )
        in

        let new_t_ =
          match is_zmap with
          | true ->
            (
              match is_masscan with
              | true -> Zmap_masscan
              | false -> Zmap
            )
          | false ->
            (
              match is_masscan with
              | true -> Masscan
              | false -> None
            )
        in
        
        (* print_endline *)
        (*   (sprintf *)
        (*      "Packet_fingerprint: of_packet_data_for_metrics: is_zmap: %b ; is_masscan: %b : new_t: %s" *)
        (*      is_zmap *)
        (*      is_masscan *)
        (*      (to_string new_t_) *)
        (*   ); *)

        new_t_
      )
    | Packet_data_for_metrics.IPV6 ipv6_data_for_metrics ->
      None
    | Packet_data_for_metrics.Other ->
      None
  )

let default_value () = None

let to_float t = float_of_int (to_int t)

(* let hash = Hashtbl.hash *)

(* let min = return_null_value () *)
(* let max = return_null_value () *)

(* let pred t = t *)
(* let succ t = t *)

(* let sub t1 t2 = *)
(*   Batteries.Int.sub *)
(*     (to_int t1) *)
(*     (to_int t2) *)
