
open Printf

open Sexplib.Std

open Ipv4
open Tcp
open Udp
open Icmp

open Key_occurrence_distribution_instantiations

type t =
  {
    mutable nb_tcp_packets : int;
    
    mutable nb_urg_packets : int;
    mutable nb_ack_packets : int;
    mutable nb_psh_packets : int;
    mutable nb_rst_packets : int;
    mutable nb_syn_packets : int;
    mutable nb_fin_packets : int;
  }
[@@deriving compare, sexp]

let new_t
    nb_tcp_packets

    nb_urg_packets
    nb_ack_packets
    nb_psh_packets
    nb_rst_packets
    nb_syn_packets
    nb_fin_packets
  =
  {
    nb_tcp_packets;

    nb_urg_packets;
    nb_ack_packets;
    nb_psh_packets;
    nb_rst_packets;
    nb_syn_packets;
    nb_fin_packets;
  }

let new_empty_t () =
  new_t
    0

    0
    0
    0
    0
    0
    0


let verify nb_packets error_string t =
  (
    assert(nb_packets = t.nb_tcp_packets)
  )
  
let copy t =
  new_t
    t.nb_tcp_packets

    t.nb_urg_packets
    t.nb_ack_packets
    t.nb_psh_packets
    t.nb_rst_packets
    t.nb_syn_packets
    t.nb_fin_packets

let append
    t
    t_to_append
  =
  (
    t.nb_tcp_packets <- t.nb_tcp_packets + t_to_append.nb_tcp_packets;

    t.nb_urg_packets <- t.nb_urg_packets + t_to_append.nb_urg_packets;
    t.nb_ack_packets <- t.nb_ack_packets + t_to_append.nb_ack_packets;
    t.nb_psh_packets <- t.nb_psh_packets + t_to_append.nb_psh_packets;
    t.nb_rst_packets <- t.nb_rst_packets + t_to_append.nb_rst_packets;
    t.nb_syn_packets <- t.nb_syn_packets + t_to_append.nb_syn_packets;
    t.nb_fin_packets <- t.nb_fin_packets + t_to_append.nb_fin_packets;    
  )

let fusion
    t_1
    t_2
  =
  (
    let nb_tcp_packets = t_1.nb_tcp_packets + t_2.nb_tcp_packets in

    let nb_urg_packets = t_1.nb_urg_packets + t_2.nb_urg_packets in
    let nb_ack_packets = t_1.nb_ack_packets + t_2.nb_ack_packets in
    let nb_psh_packets = t_1.nb_psh_packets + t_2.nb_psh_packets in
    let nb_rst_packets = t_1.nb_rst_packets + t_2.nb_rst_packets in
    let nb_syn_packets = t_1.nb_syn_packets + t_2.nb_syn_packets in
    let nb_fin_packets = t_1.nb_fin_packets + t_2.nb_fin_packets in

    new_t
      nb_tcp_packets

      nb_urg_packets
      nb_ack_packets
      nb_psh_packets
      nb_rst_packets
      nb_syn_packets
      nb_fin_packets
  )

let of_tcp_data_for_metrics
    tcp_data_for_metrics
  =
  new_t
    1

    (if tcp_data_for_metrics.Tcp_data_for_metrics.urg then 1 else 0)
    (if tcp_data_for_metrics.Tcp_data_for_metrics.ack then 1 else 0)
    (if tcp_data_for_metrics.Tcp_data_for_metrics.psh then 1 else 0)
    (if tcp_data_for_metrics.Tcp_data_for_metrics.rst then 1 else 0)
    (if tcp_data_for_metrics.Tcp_data_for_metrics.syn then 1 else 0)
    (if tcp_data_for_metrics.Tcp_data_for_metrics.fin then 1 else 0)
    
let update
    t
    tcp_for_metrics
  =
  (
    t.nb_tcp_packets <- t.nb_tcp_packets + 1;

    if tcp_for_metrics.Tcp_data_for_metrics.urg then
      t.nb_urg_packets <- t.nb_urg_packets + 1;

    if tcp_for_metrics.Tcp_data_for_metrics.ack then
      t.nb_ack_packets <- t.nb_ack_packets + 1;

    if tcp_for_metrics.Tcp_data_for_metrics.psh then
      t.nb_psh_packets <- t.nb_psh_packets + 1;

    if tcp_for_metrics.Tcp_data_for_metrics.rst then
      t.nb_rst_packets <- t.nb_rst_packets + 1;

    if tcp_for_metrics.Tcp_data_for_metrics.syn then
      t.nb_syn_packets <- t.nb_syn_packets + 1;    

    if tcp_for_metrics.Tcp_data_for_metrics.fin then
      t.nb_fin_packets <- t.nb_fin_packets + 1;
  )

let to_string
    t
  =
  sprintf
    "5tuple TCP:\nNb packets: %d\nURG: %d\nACK: %d\nPSH: %d\nRST: %d\nSYN: %d\nFIN: %d"
    t.nb_tcp_packets

    t.nb_urg_packets
    t.nb_ack_packets
    t.nb_psh_packets
    t.nb_rst_packets
    t.nb_syn_packets
    t.nb_fin_packets
