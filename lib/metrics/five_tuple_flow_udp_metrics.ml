
open Printf

open Key_occurrence_distribution_instantiations

type t =
  {
    mutable nb_udp_packets : int;
    mutable nb_udp_datagrams : int;
    
    data_length_distribution : Int_distribution.C.t;
  }
with compare

let new_t
    nb_udp_packets
    nb_udp_datagrams
    data_length_distribution
    =
  {
    nb_udp_packets;
    nb_udp_datagrams;
    data_length_distribution;
  }

let new_empty_t () =
  new_t
    0
    0
    (Int_distribution.C.new_single_t ())

let to_string
    t
  =
  sprintf
    "5tuple UDP:\nNb packets: %d\nNb datagrams: %d\nData length: %s"
    t.nb_udp_packets
    t.nb_udp_datagrams
    (Int_distribution.C.to_string_full t.data_length_distribution)

let verify nb_packets error_string t =
  (
    assert(nb_packets = t.nb_udp_packets);

    let number_data_length = Int_distribution.C.get_total_nb_occurrence t.data_length_distribution in
    if number_data_length <> t.nb_udp_packets then
      (
        print_endline
          (sprintf
             "[Five_tuple_udp_metrics]: verify: inconsistency between number of data length occurence (%d) and total number of UDP packets (%d):\n%s\n\n%s\n\n"
             number_data_length
             t.nb_udp_packets
             (to_string
                t
             )
             error_string
          );
        assert(false)
      );
  )
  
let copy t =
  new_t
    t.nb_udp_packets
    t.nb_udp_datagrams
    (Int_distribution.C.copy t.data_length_distribution)

let append
    t
    t_to_append
  =
  (
    t.nb_udp_packets <- t.nb_udp_packets + t_to_append.nb_udp_packets;
    t.nb_udp_datagrams <- t.nb_udp_datagrams + t_to_append.nb_udp_datagrams;

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

    let data_length_distribution =
      Int_distribution.C.fusion
        t_1.data_length_distribution
        t_2.data_length_distribution
    in

    let new_t =
      new_t
        nb_udp_packets
        nb_udp_datagrams
        data_length_distribution
    in

    (* verify *)
    (*   nb_udp_packets *)
    (*   "" *)
    (*   new_t; *)

    new_t
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

        Int_distribution.C.add_single_occurrence t.data_length_distribution udp_for_metrics.Udp_data_for_metrics.data_length;
      )
      
    (* verify *)
    (*   t.nb_udp_packets *)
    (*   "" *)
    (* t; *)
  )
