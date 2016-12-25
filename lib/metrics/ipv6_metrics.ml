
open Printf

open Sexplib.Std

type t =
  {
    mutable nb_packets : int;
  }
[@@deriving compare, sexp]

let new_t
    nb_packets
  =
  {    
    nb_packets;
  }

let new_empty_t () =
  new_t
    0

let copy t =
  new_t
    t.nb_packets

let append
    t
    t_to_append
  =
  (
    t.nb_packets <- t.nb_packets + t_to_append.nb_packets;
  )

let fusion
    t_1
    t_2
  =
  (
    let nb_packets = t_1.nb_packets + t_2.nb_packets in

    new_t
      nb_packets
  )

let update
    t
    ipv6_data_for_metrics
  =
  (    
    t.nb_packets <- t.nb_packets + 1;
  )

let to_string
    t
  =
  sprintf
    "IPv6: nb packets: %d"
    t.nb_packets
