
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
      t.nb_packets <-
      t.nb_packets + t_to_append.nb_packets;
  )

let fusion
    t_1
    t_2
  =
  (
    let nb_packet =
      t_1.nb_packets
      +
      t_2.nb_packets
    in

    new_t
      nb_packet
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
  sprintf "GRE: nb packets: %d"
    t.nb_packets
