
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

    src_port : Int_distribution.C.t;
    dst_port : Int_distribution.C.t;
    
    mutable nb_urg_packets : int;
    mutable nb_ack_packets : int;
    mutable nb_psh_packets : int;
    mutable nb_rst_packets : int;
    mutable nb_syn_packets : int;
    mutable nb_fin_packets : int;
  }
with compare, sexp

let new_t
    nb_tcp_packets

    src_port
    dst_port

    nb_urg_packets
    nb_ack_packets
    nb_psh_packets
    nb_rst_packets
    nb_syn_packets
    nb_fin_packets
  =
  {
    nb_tcp_packets;

    src_port;
    dst_port;

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

    (Int_distribution.C.new_single_t ())
    (Int_distribution.C.new_single_t ())

    0
    0
    0
    0
    0
    0

let verify t =
  (
    let number_src_port = Int_distribution.C.get_total_nb_occurrence t.src_port in
    assert(number_src_port = t.nb_tcp_packets);
    let number_dst_port = Int_distribution.C.get_total_nb_occurrence t.dst_port in
    assert(number_dst_port = t.nb_tcp_packets);
  )
  
let copy t =
  new_t
    t.nb_tcp_packets

    (Int_distribution.C.copy t.src_port)
    (Int_distribution.C.copy t.dst_port)

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

    Int_distribution.C.append
      t.src_port
      t_to_append.src_port;
    Int_distribution.C.append
      t.dst_port
      t_to_append.dst_port;

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

    let src_port = Int_distribution.C.fusion t_1.src_port t_2.src_port in
    let dst_port = Int_distribution.C.fusion t_1.dst_port t_2.dst_port in

    let nb_urg_packets = t_1.nb_urg_packets + t_2.nb_urg_packets in
    let nb_ack_packets = t_1.nb_ack_packets + t_2.nb_ack_packets in
    let nb_psh_packets = t_1.nb_psh_packets + t_2.nb_psh_packets in
    let nb_rst_packets = t_1.nb_rst_packets + t_2.nb_rst_packets in
    let nb_syn_packets = t_1.nb_syn_packets + t_2.nb_syn_packets in
    let nb_fin_packets = t_1.nb_fin_packets + t_2.nb_fin_packets in

    new_t
      nb_tcp_packets

      src_port
      dst_port

      nb_urg_packets
      nb_ack_packets
      nb_psh_packets
      nb_rst_packets
      nb_syn_packets
      nb_fin_packets
  )

let fusion_detailed_metrics = fusion

let update
    t
    tcp_for_metrics
  =
  (
    t.nb_tcp_packets <- t.nb_tcp_packets + 1;

    Int_distribution.C.add_single_occurrence t.src_port tcp_for_metrics.Tcp_data_for_metrics.source_port;
    Int_distribution.C.add_single_occurrence t.dst_port tcp_for_metrics.Tcp_data_for_metrics.destination_port;

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
  sprintf "TCP: nb packets: %d\nSrc port: %s\nDst port: %s\n"
    t.nb_tcp_packets
    (Int_distribution.C.to_string t.src_port)
    (Int_distribution.C.to_string t.dst_port)
  ^
  sprintf
    "nb urg packets: %d\nnb ack packet: %d\nnb psh packet: %d\nnb rst packet: %d\nnb syn packet: %d\nnb fin packet: %d"
    t.nb_urg_packets
    t.nb_ack_packets
    t.nb_psh_packets
    t.nb_rst_packets
    t.nb_syn_packets
    t.nb_fin_packets
