
open Printf

open Sexplib.Std

open Ipv4
open Tcp
open Udp
open Icmp

open Key_occurrence_distribution_instantiations

type t =
  {
    mutable nb_icmp_tcp_packets : int;

    src_port : Int_distribution.C.t;
    dst_port : Int_distribution.C.t;
  }
with compare, sexp

let new_t
    nb_icmp_tcp_packets

    src_port
    dst_port
  =
  {
    nb_icmp_tcp_packets = nb_icmp_tcp_packets;

    src_port = src_port;
    dst_port = dst_port;
  }

let new_single_t () =
  new_t
    0

    (* src_port *)
    (Int_distribution.C.new_single_t ())
    (* dst_port *)
    (Int_distribution.C.new_single_t ())

let copy t =
  new_t
    t.nb_icmp_tcp_packets

    (Int_distribution.C.copy t.src_port)
    (Int_distribution.C.copy t.dst_port)

let append
    t
    t_to_append
  =
  (
    t.nb_icmp_tcp_packets <- t.nb_icmp_tcp_packets + t_to_append.nb_icmp_tcp_packets;
    (* IntSimple.append *)
    (*   t_1.nb_icmp_tcp_packets *)
    (*   t_2.nb_icmp_tcp_packets; *)

    Int_distribution.C.append
      t.src_port
      t_to_append.src_port;

    Int_distribution.C.append
      t.dst_port
      t_to_append.dst_port;
  )

let fusion
    t_1
    t_2
  =
  (
    let nb_icmp_tcp_packets =
      (* IntSimple.fusion *)
      t_1.nb_icmp_tcp_packets
      +
      t_2.nb_icmp_tcp_packets
    in

    let src_port =
      Int_distribution.C.fusion
        t_1.src_port
        t_2.src_port
    in
    let dst_port =
      Int_distribution.C.fusion
        t_1.dst_port
        t_2.dst_port
    in

    new_t
      nb_icmp_tcp_packets

      src_port
      dst_port
  )

let fusion_detailed_metrics = fusion

let update
    t
    icmp_tcp_data_for_metrics
  =
  (
    t.nb_icmp_tcp_packets <- t.nb_icmp_tcp_packets + 1;

    Int_distribution.C.add_single_occurrence t.src_port icmp_tcp_data_for_metrics.Icmp_tcp_data_for_metrics.source_port;
    Int_distribution.C.add_single_occurrence t.dst_port icmp_tcp_data_for_metrics.Icmp_tcp_data_for_metrics.destination_port;
  )

let to_string
    t
  =
  sprintf "ICMP-TCP: nb packets: %d\nSrc port: %s\nDst port: %s"
    t.nb_icmp_tcp_packets
    (Int_distribution.C.to_string_full t.src_port)
    (Int_distribution.C.to_string_full t.dst_port)

let verify t =
  let number_src_port = Int_distribution.C.get_total_nb_occurrence t.src_port in
  assert(number_src_port = t.nb_icmp_tcp_packets);
  let number_dst_port = Int_distribution.C.get_total_nb_occurrence t.dst_port in
  assert(number_dst_port = t.nb_icmp_tcp_packets);
