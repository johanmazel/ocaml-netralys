
open Printf

open Sexplib.Std

open Ipv4
open Tcp
open Udp
open Icmp

open Key_occurrence_distribution_instantiations

type t =
  {
    mutable nb_packets : int;
    icmp_encapsulated_metrics : Icmp_encapsulated_metrics.t;
  }
[@@deriving compare, sexp]

let new_t
    nb_packets
    icmp_encapsulated_metrics
    =
  {
    nb_packets;
    icmp_encapsulated_metrics
  }

let new_single_t () =
  new_t
    0
    (Icmp_encapsulated_metrics.new_single_t ())

let to_string
    t
    =
  sprintf "ICMP TE: nb packets: %d\n%s"
    t.nb_packets

    (Icmp_encapsulated_metrics.to_string t.icmp_encapsulated_metrics)

let verify error_string t =
  (    
    let number_of_packets = t.nb_packets in

    Icmp_encapsulated_metrics.verify 
      number_of_packets
      error_string
      t.icmp_encapsulated_metrics;
  )

let copy t = 
  new_t
    t.nb_packets

    (Icmp_encapsulated_metrics.copy t.icmp_encapsulated_metrics)

let append
    t
    t_to_append
  =
  (
    t.nb_packets <- t.nb_packets + t_to_append.nb_packets;
    
    Icmp_encapsulated_metrics.append 
      t.icmp_encapsulated_metrics
      t_to_append.icmp_encapsulated_metrics;
  )

let fusion
    t1
    t2
  =
  (
    let nb_packets = t1.nb_packets + t2.nb_packets in

    let icmp_encapsulated_metrics =
      Icmp_encapsulated_metrics.fusion
        t1.icmp_encapsulated_metrics
        t2.icmp_encapsulated_metrics
    in

    let t =   
      new_t
        nb_packets

        icmp_encapsulated_metrics
    in

    t
  )

let update
    t
    (code, source_address , destination_address , icmp_transport_layer_data_for_metrics)
    =
  (
    t.nb_packets <- t.nb_packets + 1;

    Icmp_encapsulated_metrics.update
      t.icmp_encapsulated_metrics
      (Admd.Ipaddr_sb.of_ipaddr source_address, Admd.Ipaddr_sb.of_ipaddr destination_address, icmp_transport_layer_data_for_metrics);
  )
