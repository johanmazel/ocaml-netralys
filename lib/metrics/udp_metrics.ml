
open Printf

open Sexplib.Std

open Ipv4
open Tcp
open Udp
open Icmp

open Key_occurrence_distribution_instantiations

type t =
  {
    mutable nb_udp_packets : int;
    mutable nb_udp_datagrams : int;

    src_port : Int_distribution.C.t;
    dst_port : Int_distribution.C.t;
    
    data_length_distribution : Int_distribution.C.t;
  }
[@@deriving compare, sexp]

let new_t
    nb_udp_packets
    nb_udp_datagrams
    
    src_port
    dst_port

    data_length_distribution
    =
  {    
    nb_udp_packets;
    nb_udp_datagrams;
    
    src_port;
    dst_port;

    data_length_distribution;
  }

let new_single_t () =
  new_t
    0
    0
    
    (Int_distribution.C.new_single_t ())
    (Int_distribution.C.new_single_t ())
    
    (Int_distribution.C.new_single_t ())

let copy t =
  new_t
    t.nb_udp_packets
    t.nb_udp_datagrams

    (Int_distribution.C.copy t.src_port)
    (Int_distribution.C.copy t.dst_port)
    
    (Int_distribution.C.copy t.data_length_distribution)

let append
    t
    t_to_append
    =
  (
    t.nb_udp_packets <- t.nb_udp_packets + t_to_append.nb_udp_packets;
    t.nb_udp_datagrams <- t.nb_udp_datagrams + t_to_append.nb_udp_datagrams;

    Int_distribution.C.append
      t.src_port
      t_to_append.src_port;

    Int_distribution.C.append
      t.dst_port
      t_to_append.dst_port;
    
    Int_distribution.C.append
      t.data_length_distribution
      t_to_append.data_length_distribution;
  )

let fusion
    t_1
    t_2
  =
  (
    let nb_udp_packets = t_1.nb_udp_packets + t_2.nb_udp_packets in
    let nb_udp_datagrams = t_1.nb_udp_datagrams + t_2.nb_udp_datagrams in

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

    let data_length_distribution =
      Int_distribution.C.fusion
        t_1.data_length_distribution
        t_2.data_length_distribution
    in

    new_t
      nb_udp_packets
      nb_udp_datagrams
      
      src_port
      dst_port

      data_length_distribution
  )

let update
    t
    
    fragmentation
    first_fragment
    
    udp_for_metrics
  =
  (
    t.nb_udp_packets <- t.nb_udp_packets + 1;

    if (fragmentation && first_fragment) || fragmentation == false then
      (
        t.nb_udp_datagrams <- t.nb_udp_datagrams + 1;

        Int_distribution.C.add_single_occurrence t.src_port udp_for_metrics.Udp_data_for_metrics.source_port;
        Int_distribution.C.add_single_occurrence t.dst_port udp_for_metrics.Udp_data_for_metrics.destination_port;

        Int_distribution.C.add_single_occurrence t.data_length_distribution udp_for_metrics.Udp_data_for_metrics.data_length;
      )
  )

let to_string
    t
  =
  sprintf "UDP: nb packets: %d\nSrc_port: %s\nDst port: %s\nData length: %s"
    t.nb_udp_packets
    (Int_distribution.C.to_string_full t.src_port)
    (Int_distribution.C.to_string_full t.dst_port)
    (Int_distribution.C.to_string_full t.data_length_distribution)

let verify t =
  let number_src_port = Int_distribution.C.get_total_nb_occurrence t.src_port in
  assert(number_src_port <= t.nb_udp_packets);
  let number_dst_port = Int_distribution.C.get_total_nb_occurrence t.dst_port in
  assert(number_dst_port <= t.nb_udp_packets);

  let number_data_length = Int_distribution.C.get_total_nb_occurrence t.data_length_distribution in
  if number_data_length > t.nb_udp_packets then
    (
      print_endline
        (sprintf
           "[Udp_metrics]: verify: number of data length occurence (%d) > number of UDP packets (%d):\n%s"
           number_data_length
           t.nb_udp_packets
           (to_string
              t
           )
        );
      assert(false)
    );
