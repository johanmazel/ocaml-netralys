
open Printf

open Sexplib.Std

open Key_occurrence_distribution_instantiations
    
type t =
  {
    mutable nb_packets : int;
    code : Int_distribution.C.t;
    icmp_encapsulated_metrics : Icmp_encapsulated_metrics.t;
  }
with compare, sexp

let new_t
    nb_packets
    code
    icmp_encapsulated_metrics
    =
  {
    nb_packets;
    code;
    icmp_encapsulated_metrics
  }

let new_single_t () =
  new_t
    0
    (Int_distribution.C.new_single_t ())
    (Icmp_encapsulated_metrics.new_single_t ())

let to_string
    t
    =
  sprintf "ICMP DU: nb packets: %d\nDU code: %s\n%s"
    t.nb_packets

    (Int_distribution.C.to_string_full t.code)

    (Icmp_encapsulated_metrics.to_string t.icmp_encapsulated_metrics)

let verify error_string t =
  (    
    let number_of_packets = t.nb_packets in
    let number_code = Int_distribution.C.get_total_nb_occurrence t.code in
    if number_code <> number_of_packets then
      (
        print_endline
          (sprintf
             "Icmp_du_metrics: verify: inconsistency between total number of icmp du src_addr (%d) and total number of code (%d):\n%s\n\n\n%s"
             number_code
             number_of_packets
             (to_string
                t
             )
             error_string
          );
        assert(false)
      );

    Icmp_encapsulated_metrics.verify 
      number_of_packets
      error_string
      t.icmp_encapsulated_metrics;
  )

let copy t = 
  new_t
    t.nb_packets

    (Int_distribution.C.copy t.code)
    (Icmp_encapsulated_metrics.copy t.icmp_encapsulated_metrics)

let append
    t
    t_to_append
    =
  (
    t.nb_packets <- t.nb_packets + t_to_append.nb_packets;

    Int_distribution.C.append
      t.code
      t_to_append.code;

    Icmp_encapsulated_metrics.append 
      t.icmp_encapsulated_metrics
      t_to_append.icmp_encapsulated_metrics;
  )

let fusion
    t1
    t2
    =
  (
    let nb_packet = t1.nb_packets + t2.nb_packets in

    let code =
      Int_distribution.C.fusion
      t1.code
      t2.code
    in
    
    let icmp_encapsulated_metrics =
      Icmp_encapsulated_metrics.fusion
      t1.icmp_encapsulated_metrics
      t2.icmp_encapsulated_metrics
    in

    let t =   
      new_t
      nb_packet

      code

      icmp_encapsulated_metrics
    in

    t
  )

let update
    t
    (code, source_ipaddr, destination_ipaddr, icmp_transport_layer_data_for_metrics)
  =
  (
    t.nb_packets <- t.nb_packets + 1;

    Int_distribution.C.add_single_occurrence t.code code;

    Icmp_encapsulated_metrics.update
      t.icmp_encapsulated_metrics
      (
        Admd.Ipaddr_sb.of_ipaddr source_ipaddr,
        Admd.Ipaddr_sb.of_ipaddr destination_ipaddr,
        icmp_transport_layer_data_for_metrics
      );
  )
