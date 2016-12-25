
open Printf

open Batteries

open Ipv4
open Tcp
open Udp
open Icmp

open Key_occurrence_distribution_instantiations

type t =
  {
    mutable nb_packets : int;
  }
[@@deriving compare]

let new_t
    nb_packets
  =
  {
    nb_packets;
  }

let new_empty_t () =
  new_t
    0

let verify nb_packets error_string t =
  (
    assert(nb_packets = t.nb_packets)
  )
  
let copy t =
  new_t
    t.nb_packets

let append
    t_1
    t_2
  =
  (
    let nb_packets = t_1.nb_packets + t_2.nb_packets in

    new_t
      nb_packets
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
    =
  (
    t.nb_packets <- t.nb_packets + 1;
  )

let to_string
    t
  =
  sprintf
    "5tuple GRE:\nNb packets: %d"
    t.nb_packets

