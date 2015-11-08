
open Printf

open Sexplib.Std

module HT = BatHashtbl

type t =
  {
    src_addr : Ipaddr.t;
    dst_addr : Ipaddr.t;
    protocol : Transport_protocol_for_metrics.t;
    src_port : int;
    dst_port : int;
  }
with compare, sexp

let new_t 
    src_addr
    dst_addr
    protocol
    src_port
    dst_port
    =
  {
    src_addr = src_addr;
    dst_addr = dst_addr;
    protocol = protocol;
    src_port = src_port;
    dst_port = dst_port;
  }

let to_string t = 
  sprintf
    "%s-%s:%s:%d-%d"
    (Ipaddr.to_string t.src_addr)
    (Ipaddr.to_string t.dst_addr)
    (Transport_protocol_for_metrics.to_string t.protocol)
    t.src_port
    t.dst_port

let to_filename_prefix t = 
  "flow_" ^ (to_string t)

let of_packet_data_for_metrics
    packet_data_for_metrics
  =
  (
    match packet_data_for_metrics.Packet_data_for_metrics.pdu_t with
    | Packet_data_for_metrics.IPV4 ipv4_for_metrics ->
      (
        let src_addr = ipv4_for_metrics.Ipv4_data_for_metrics.source_address in
        let dst_addr = ipv4_for_metrics.Ipv4_data_for_metrics.destination_address in

        match ipv4_for_metrics.Ipv4_data_for_metrics.pdu_t with
        | Ipv4_data_for_metrics.ICMP icmp_data_for_metrics ->
          (
            new_t
              (Ipaddr.V4 src_addr)
              (Ipaddr.V4 dst_addr)
              Transport_protocol_for_metrics.ICMP
              0
              0
          )
        | Ipv4_data_for_metrics.TCP tcp_for_metrics ->
          (      
            new_t
              (Ipaddr.V4 src_addr)
              (Ipaddr.V4 dst_addr)
              Transport_protocol_for_metrics.TCP
              tcp_for_metrics.Tcp_data_for_metrics.source_port
              tcp_for_metrics.Tcp_data_for_metrics.destination_port
          )
        | Ipv4_data_for_metrics.UDP udp_for_metrics ->
          (
            new_t
              (Ipaddr.V4 src_addr)
              (Ipaddr.V4 dst_addr)
              Transport_protocol_for_metrics.UDP
              udp_for_metrics.Udp_data_for_metrics.source_port
              udp_for_metrics.Udp_data_for_metrics.destination_port
          )
        | Ipv4_data_for_metrics.IPv6 ipv6_data_for_metrics ->
          (
            new_t
              (Ipaddr.V4 src_addr)
              (Ipaddr.V4 dst_addr)
              Transport_protocol_for_metrics.IPv6
              0
              0
          )
        | Ipv4_data_for_metrics.GRE ->
          (
            new_t
              (Ipaddr.V4 src_addr)
              (Ipaddr.V4 dst_addr)
              Transport_protocol_for_metrics.GRE
              0
              0
          )
        | Ipv4_data_for_metrics.Other code -> 
          failwith "Five_tuple_flow: of_packet_data_for_metrics: unexpected protocol over IP"
      )
    | Packet_data_for_metrics.IPV6 ipv6_for_metrics ->
      (
        let src_addr = ipv6_for_metrics.Ipv6_data_for_metrics.source_address in
        let dst_addr = ipv6_for_metrics.Ipv6_data_for_metrics.destination_address in

        match ipv6_for_metrics.Ipv6_data_for_metrics.pdu_t with
        | Ipv6_data_for_metrics.ICMP icmp_data_for_metrics ->
          (
            new_t
              (Ipaddr.V6 src_addr)
              (Ipaddr.V6 dst_addr)
              Transport_protocol_for_metrics.ICMP
              0
              0
          )
        | Ipv6_data_for_metrics.TCP tcp_for_metrics ->
          (
            new_t
              (Ipaddr.V6 src_addr)
              (Ipaddr.V6 dst_addr)
              Transport_protocol_for_metrics.TCP
              tcp_for_metrics.Tcp_data_for_metrics.source_port
              tcp_for_metrics.Tcp_data_for_metrics.destination_port
          )
        | Ipv6_data_for_metrics.UDP udp_for_metrics ->
          (
            new_t
              (Ipaddr.V6 src_addr)
              (Ipaddr.V6 dst_addr)
              Transport_protocol_for_metrics.UDP
              udp_for_metrics.Udp_data_for_metrics.source_port
              udp_for_metrics.Udp_data_for_metrics.destination_port
          )
        | Ipv6_data_for_metrics.GRE ->
          (
            new_t
              (Ipaddr.V6 src_addr)
              (Ipaddr.V6 dst_addr)
              Transport_protocol_for_metrics.GRE
              0
              0
          )
        | Ipv6_data_for_metrics.ICMP6 icmp6_data_for_metrics ->
          (
            new_t
              (Ipaddr.V6 src_addr)
              (Ipaddr.V6 dst_addr)
              Transport_protocol_for_metrics.ICMPv6
              0
              0
          )
        | Ipv6_data_for_metrics.Other code ->
          failwith "Five_tuple_flow: of_packet_data_for_metrics: unexpected protocol over IP"
      )
    | Packet_data_for_metrics.Other -> 
      failwith "Five_tuple_flow: of_packet_data_for_metrics: not IPV4"
  )

let to_five_tuple_option
    t
    =
  (Some t.src_addr)
    ,
  (Some t.dst_addr)
    ,
  (Some  t.protocol)
    , 
  (if t.src_port = 0 then
      None
   else
      Some t.src_port
  )
    , 
  (if t.dst_port =0 then
      None
   else
      Some t.dst_port
  )
  
let to_five_tuple
    t
    =
  (
    t.src_addr,
    t.dst_addr,
    t.protocol,
    t.src_port,
    t.dst_port
  )

let hash = HT.hash
