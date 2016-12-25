
open Printf


open Sexplib.Std
open Bin_prot.Std
       
open Ipv4
open Tcp
open Udp
open Icmp
       
open Key_occurrence_distribution_instantiations

type t =
  {
    mutable nb_icmp_icmp_packets : int;
    mutable nb_echorequestreply_packets : int;
    mutable nb_destination_unreachable_packets : int;

    icmp_type : Int_distribution.C.t;

    icmp_du_code : Int_distribution.C.t;
  }
[@@deriving compare, sexp]

let new_t
    nb_icmp_icmp_packets
    nb_echorequestreply_packets
    nb_destination_unreachable_packets

    icmp_type

    icmp_du_code
  =
  {    
    nb_icmp_icmp_packets;
    nb_echorequestreply_packets;
    nb_destination_unreachable_packets;

    icmp_type;

    icmp_du_code;
  }

let new_single_t () =
  new_t
    0
    0
    0

    (* icmp_type *)
    (Int_distribution.C.new_single_t ())
    (* icmp_du_code *)
    (Int_distribution.C.new_single_t ())

let copy t =
  new_t
    t.nb_icmp_icmp_packets
    t.nb_echorequestreply_packets
    t.nb_destination_unreachable_packets

    (Int_distribution.C.copy t.icmp_type)

    (Int_distribution.C.copy t.icmp_du_code)

let append
    t
    t_to_append
  =
  (    
    t.nb_icmp_icmp_packets <- t.nb_icmp_icmp_packets + t_to_append.nb_icmp_icmp_packets;
    t.nb_echorequestreply_packets <- t.nb_echorequestreply_packets + t_to_append.nb_echorequestreply_packets;
    t.nb_destination_unreachable_packets <-
      t.nb_destination_unreachable_packets + t_to_append.nb_destination_unreachable_packets;

    Int_distribution.C.append
      t.icmp_type
      t_to_append.icmp_type;    

    Int_distribution.C.append
      t.icmp_du_code
      t_to_append.icmp_du_code;
  )

let fusion
    t1
    t2
  =
  (
    (* let nb_icmp_packet = *)
    (*   IntSimple.fusion *)
    (*     t1.nb_icmp_icmp_packets *)
    (*     t2.nb_icmp_icmp_packets *)
    (* in *)
    (* let nb_echorequestreply_packet = *)
    (*   IntSimple.fusion *)
    (*     t1.nb_echorequestreply_packet *)
    (*     t2.nb_echorequestreply_packet *)
    (* in *)
    (* let nb_destination_unreachable_packet = *)
    (*   IntSimple.fusion *)
    (*     t1.nb_destination_unreachable_packet *)
    (*     t2.nb_destination_unreachable_packet *)
    (* in *)
    let nb_icmp_icmp_packets =
      t1.nb_icmp_icmp_packets + t2.nb_icmp_icmp_packets
    in
    let nb_echorequestreply_packets = t1.nb_echorequestreply_packets + t2.nb_echorequestreply_packets in
    let nb_destination_unreachable_packets =
      t1.nb_destination_unreachable_packets + t2.nb_destination_unreachable_packets
    in

    let icmp_type =
      Int_distribution.C.fusion
        t1.icmp_type
        t2.icmp_type
    in

    let icmp_du_code =
      Int_distribution.C.fusion
        t1.icmp_du_code
        t2.icmp_du_code
    in

    new_t
      nb_icmp_icmp_packets
      nb_echorequestreply_packets
      nb_destination_unreachable_packets

      icmp_type

      icmp_du_code
  )

let update
    t
    icmp_icmp_data_for_metrics
  =
  (
    (* IntSimple.add_value t.nb_icmp_icmp_packets 1; *)
    t.nb_icmp_icmp_packets <- t.nb_icmp_icmp_packets + 1;

    match icmp_icmp_data_for_metrics with
    | Icmp_icmp_data_for_metrics.Echo_request ->
      (
        Int_distribution.C.add_single_occurrence t.icmp_type 8;
      (* IntSimple.add_value t.nb_echorequestreply_packet 1 *)
        t.nb_echorequestreply_packets <- t.nb_echorequestreply_packets + 1;
      )
    | Icmp_icmp_data_for_metrics.Echo_reply -> 
      (
        Int_distribution.C.add_single_occurrence t.icmp_type 0;
      (* IntSimple.add_value t.nb_echorequestreply_packet 1 *)
        t.nb_echorequestreply_packets <- t.nb_echorequestreply_packets + 1;
      )
    | Icmp_icmp_data_for_metrics.Destination_unreachable code->
      (
        Int_distribution.C.add_single_occurrence t.icmp_type 3;

        (* IntSimple.add_value t.nb_destination_unreachable_packet 1; *)
        t.nb_destination_unreachable_packets <- t.nb_destination_unreachable_packets + 1;

        Int_distribution.C.add_single_occurrence t.icmp_du_code code;
      )
    | Icmp_icmp_data_for_metrics.Other ->
      (
      Int_distribution.C.add_single_occurrence t.icmp_type (-1)
      )
  )

let to_string
    t
  =
  sprintf
    "ICMP-ICMP: nb packets: %d\nEcho request-replay:  %d\nDU: %d\nDU code: %s"
    t.nb_icmp_icmp_packets
    t.nb_echorequestreply_packets
    t.nb_destination_unreachable_packets

    (Int_distribution.C.to_string_full t.icmp_du_code)

let verify t =
  let number_type = Int_distribution.C.get_total_nb_occurrence t.icmp_type in
  if number_type <> t.nb_icmp_icmp_packets then
    (
      print_endline
      (sprintf
         "Icmp_icmp_metrics: verify: inconsistency between total number of occurence of type (%d) and total number of icmp packets (%d):\n%s"
         number_type
         t.nb_icmp_icmp_packets
         (to_string
            t
         )
      );
      assert(false)
    );

  let number_du = Int_distribution.C.get_total_nb_occurrence t.icmp_du_code in
  assert(number_du = t.nb_destination_unreachable_packets);
