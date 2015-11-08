
open Printf

open Batteries

open Sexplib.Std

open Ipv4
open Tcp
open Udp
open Icmp

open Key_occurrence_distribution_instantiations
open Network_traffic_metric_instantiations

type t =
  {
    src_addr : Ipaddr_distribution.C.t;
    dst_addr : Ipaddr_distribution.C.t;

    mutable icmp_icmp_metrics_option : Icmp_icmp_metrics.t option;
    mutable icmp_tcp_metrics_option : Icmp_tcp_metrics.t option;
    mutable icmp_udp_metrics_option : Udp_metrics.t option;
  }
with compare, sexp

let new_t
    src_addr
    dst_addr

    icmp_icmp_metrics_option
    icmp_tcp_metrics_option
    icmp_udp_metrics_option
    =
  {
    src_addr;
    dst_addr;

    icmp_icmp_metrics_option;
    icmp_tcp_metrics_option;
    icmp_udp_metrics_option;
  }

let new_single_t () =
  new_t
    (Ipaddr_distribution.C.new_single_t ())
    (Ipaddr_distribution.C.new_single_t ())

    None
    None
    None

let to_string
    t
  =
  sprintf "Src addr: %s\nDst addr: %s\n%s"
    (Ipaddr_distribution.C.to_string_full t.src_addr)
    (Ipaddr_distribution.C.to_string_full t.dst_addr)

    (
      (match t.icmp_icmp_metrics_option with
       | None -> ""
       | Some icmp_icmp_metrics -> Icmp_icmp_metrics.to_string icmp_icmp_metrics ^ "\n"
      )
      ^
      (match t.icmp_tcp_metrics_option with
       | None -> ""
       | Some tcp_metrics -> Icmp_tcp_metrics.to_string tcp_metrics ^ "\n"
      )
      ^
      (match t.icmp_udp_metrics_option with
       | None -> ""
       | Some icmp_udp_metrics -> "ICMP-" ^ Udp_metrics.to_string icmp_udp_metrics
      )
    )

let verify number_of_packets error_string t =
  (
    let number_src_addr = Ipaddr_distribution.C.get_total_nb_occurrence t.src_addr in
    if number_src_addr <> number_of_packets then
      (
        print_endline
          (sprintf
             "Icmp_du_metrics: verify: inconsistency between number of icmp du src_addr (%d) and number of packets (%d):\n%s\n\n\n%s"
             number_src_addr
             number_of_packets
             (to_string
                t
             )
             error_string
          );
        assert(false)
      );

    let number_dst_addr = Ipaddr_distribution.C.get_total_nb_occurrence t.dst_addr in
    if number_dst_addr <> number_of_packets then
      (
        print_endline
          (sprintf
             "Icmp_du_metrics: verify: inconsistency between number of icmp du dst_addr (%d) and number of packets (%d):\n%s\n\n\n%s"
             number_dst_addr
             number_of_packets
             (to_string
                t
             )
             error_string
          );
        assert(false)
      );

    let nb_icmp_transport_layer_packet =
      (Option.map_default
         (fun icmp_icmp_metrics -> icmp_icmp_metrics.Icmp_icmp_metrics.nb_icmp_icmp_packets)
         0
         t.icmp_icmp_metrics_option
      )
      +
      (Option.map_default
         (fun icmp_tcp_metrics -> icmp_tcp_metrics.Icmp_tcp_metrics.nb_icmp_tcp_packets)
         0
         t.icmp_tcp_metrics_option
      )
      +
      (Option.map_default
         (fun udp_metrics -> udp_metrics.Udp_metrics.nb_udp_packets)
         0
         t.icmp_udp_metrics_option
      )
    in
    if number_of_packets <> nb_icmp_transport_layer_packet then
      (
        print_endline
          (sprintf
             "Icmp_du_metrics: verify: inconsistency between total number of packets (%d) and total number of icmp transport layer packets (%d):\n%s\n\n\n%s"
             number_of_packets
             nb_icmp_transport_layer_packet
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
    (* (IntSimple.copy t.nb_packets) *)

    (* (Int_distribution.C.copy t.code) *)

    (* (Int32_distribution.C.copy t.src_addr) *)
    (* (Int32_distribution.C.copy t.dst_addr) *)
    (Ipaddr_distribution.C.copy t.src_addr)
    (Ipaddr_distribution.C.copy t.dst_addr)

    (Option.map Icmp_icmp_metrics.copy t.icmp_icmp_metrics_option)
    (Option.map Icmp_tcp_metrics.copy t.icmp_tcp_metrics_option)
    (Option.map Udp_metrics.copy t.icmp_udp_metrics_option)

let append
    t1
    t2
    =
  (
    (* IntSimple.append *)
    (*   t1.nb_packets *)
    (*   t2.nb_packets; *)

    (* Int_distribution.C.append *)
    (*   t1.code *)
    (*   t2.code; *)

    (* Int32_distribution.C.append *)
    (*   t1.src_addr *)
    (*   t2.src_addr; *)
    (* Int32_distribution.C.append *)
    (*   t1.dst_addr *)
    (*   t2.dst_addr; *)
    Ipaddr_distribution.C.append
      t1.src_addr
      t2.src_addr;
    Ipaddr_distribution.C.append
      t1.dst_addr
      t2.dst_addr;

    Option_utils.append
      Icmp_icmp_metrics.append
      (fun icmp_icmp_metrics_to_append ->
          t1.icmp_icmp_metrics_option <- Some (Icmp_icmp_metrics.copy icmp_icmp_metrics_to_append)
      )
      t1.icmp_icmp_metrics_option
      t2.icmp_icmp_metrics_option;
    Option_utils.append
      Icmp_tcp_metrics.append
      (fun icmp_tcp_metrics_to_append ->
          t1.icmp_tcp_metrics_option <- Some (Icmp_tcp_metrics.copy icmp_tcp_metrics_to_append)
      )
      t1.icmp_tcp_metrics_option
      t2.icmp_tcp_metrics_option;
    Option_utils.append
      Udp_metrics.append
      (fun icmp_udp_metrics_to_append ->
          t1.icmp_udp_metrics_option <- Some (Udp_metrics.copy icmp_udp_metrics_to_append)
      )
      t1.icmp_udp_metrics_option
      t2.icmp_udp_metrics_option;
  )

let fusion
    t1
    t2
  =
  (
    (* print_endline "Icmp_encapsulated_metrics: fusion: !!!"; *)

    (* let nb_packet = *)
    (*   IntSimple.fusion *)
    (*       t1.nb_packets *)
    (*       t2.nb_packets *)
    (* in *)

    (* let code = *)
    (*   Int_distribution.C.fusion *)
    (*       t1.code *)
    (*       t2.code *)
    (* in *)

    (* let src_addr = *)
    (*   Int32_distribution.C.fusion *)
    (*       t1.src_addr *)
    (*       t2.src_addr *)
    (* in *)
    (* let dst_addr = *)
    (*   Int32_distribution.C.fusion *)
    (*       t1.dst_addr *)
    (*       t2.dst_addr *)
    (* in *)
    let src_addr =
      Ipaddr_distribution.C.fusion
        t1.src_addr
        t2.src_addr
    in
    let dst_addr =
      Ipaddr_distribution.C.fusion
        t1.dst_addr
        t2.dst_addr
    in

    let icmp_icmp_metrics_option =
      Option_utils.fusion
        Icmp_icmp_metrics.fusion
        t1.icmp_icmp_metrics_option
        t2.icmp_icmp_metrics_option
    in
    let icmp_tcp_metrics_option =
      Option_utils.fusion
        Icmp_tcp_metrics.fusion
        t1.icmp_tcp_metrics_option
        t2.icmp_tcp_metrics_option
    in
    let icmp_udp_metrics_option =
      Option_utils.fusion
        Udp_metrics.fusion
        t1.icmp_udp_metrics_option
        t2.icmp_udp_metrics_option
    in

    (* Utils.out (sprintf "Detailed_metrics: fusion_detailed_metrics:
       end");*)

    let t =   
      new_t
        (* nb_packet *)

        (* code *)

        src_addr
        dst_addr

        icmp_icmp_metrics_option
        icmp_tcp_metrics_option
        icmp_udp_metrics_option
    in

    t
  )

let update
    t
    (source_address, destination_address, icmp_transport_layer_data_for_metrics)
  =
  (
    Ipaddr_distribution.C.add_single_occurrence t.src_addr source_address;
    Ipaddr_distribution.C.add_single_occurrence t.dst_addr destination_address;

    ignore(
      match icmp_transport_layer_data_for_metrics with
      | Icmp_transport_layer_data_for_metrics.ICMP icmp_icmp_data_for_metrics ->
        let icmp_icmp_metrics = Icmp_icmp_metrics.new_single_t () in
        Icmp_icmp_metrics.update icmp_icmp_metrics icmp_icmp_data_for_metrics;

        let new_icmp_icmp_metrics_option =
          Option_utils.fusion
            Icmp_icmp_metrics.fusion
            t.icmp_icmp_metrics_option
            (Some icmp_icmp_metrics)
        in

        t.icmp_icmp_metrics_option <- new_icmp_icmp_metrics_option;
      | Icmp_transport_layer_data_for_metrics.TCP icmp_tcp_data_for_metrics ->          
        let tcp_metrics = Icmp_tcp_metrics.new_single_t () in
        Icmp_tcp_metrics.update tcp_metrics icmp_tcp_data_for_metrics;

        let new_icmp_tcp_metrics_option =
          Option_utils.fusion
            Icmp_tcp_metrics.fusion
            t.icmp_tcp_metrics_option
            (Some tcp_metrics)
        in

        t.icmp_tcp_metrics_option <- new_icmp_tcp_metrics_option;
      | Icmp_transport_layer_data_for_metrics.UDP udp_data_for_metrics ->      
        let udp_metrics = Udp_metrics.new_single_t () in
        Udp_metrics.update
          udp_metrics

          false
          false

          udp_data_for_metrics;

        let new_udp_metrics_option =
          Option_utils.fusion
            Udp_metrics.fusion
            t.icmp_udp_metrics_option
            (Some udp_metrics)
        in
        t.icmp_udp_metrics_option <- new_udp_metrics_option;
    );

    (* verify "" t; *)
  )
