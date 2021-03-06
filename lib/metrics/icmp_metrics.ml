
open Printf

open Sexplib.Std

open Ipv4
open Tcp
open Udp
open Icmp

open Key_occurrence_distribution_instantiations

type t =
  {
    icmp_type : Int_distribution.C.t;

    mutable icmp_du_metrics_option : Icmp_du_metrics.t option;
    mutable icmp_rm_metrics_option : Icmp_rm_metrics.t option;
    mutable icmp_te_metrics_option : Icmp_te_metrics.t option;
  }
[@@deriving compare, sexp]

let new_t
    icmp_type

    icmp_du_metrics_option
    icmp_rm_metrics_option
    icmp_te_metrics_option
    =
  {
    icmp_type;

    icmp_du_metrics_option;
    icmp_rm_metrics_option;
    icmp_te_metrics_option;
  }

let new_single_t () =
  new_t
    (Int_distribution.C.new_single_t ())

    None
    None
    None

let new_single_t () =
  new_t
    (Int_distribution.C.new_single_t ())

    None
    None
    None

let get_nb_packets t = Int_distribution.C.get_total_nb_occurrence t.icmp_type
    
let to_string
    t
  =
  sprintf "ICMP: nb packets: %d\nType: %s\n%s%s%s"
    (get_nb_packets t)

    (Int_distribution.C.to_string t.icmp_type)

    (Batteries.Option.map_default
       (fun icmp_du_metrics -> Icmp_du_metrics.to_string icmp_du_metrics ^ "\n")
       ""
       t.icmp_du_metrics_option
    )
    (Batteries.Option.map_default
       (fun icmp_rm_metrics -> Icmp_rm_metrics.to_string icmp_rm_metrics ^ "\n")
       ""
       t.icmp_rm_metrics_option
    )
    (Batteries.Option.map_default
       (fun icmp_te_metrics -> Icmp_te_metrics.to_string icmp_te_metrics ^ "\n")
       ""
       t.icmp_te_metrics_option
    )

let verify nb_icmp_packets error_string t =
  (
    let nb_icmp_type = Int_distribution.C.get_total_nb_occurrence t.icmp_type in
    if nb_icmp_type <> nb_icmp_packets then
      (
        print_endline
          (sprintf
             "Icmp_metrics: verify: inconsistency between number of icmp type (%d) and number of icmp packets (%d):\n%s\n\n\n%s"
             nb_icmp_type
             nb_icmp_packets
             (to_string
                t
             )
             error_string
          );
        assert(false)
      );

    let nb_destination_unreachable =
      Int_distribution.C.get_key_occurrence
        3
        t.icmp_type
    in
    let nb_icmp_du_packet =
      Batteries.Option.map_default
        (fun icmp_du_metrics -> icmp_du_metrics.Icmp_du_metrics.nb_packets)
        0
        t.icmp_du_metrics_option
    in
    if nb_icmp_du_packet <> nb_destination_unreachable then
      (
        print_endline
          (sprintf
             "Icmp_metrics: verify: inconsistency between number of icmp du type (%d) and number of icmp destination unreachable packets (%d):\n%s\n\n\n%s"
             nb_icmp_du_packet
             nb_destination_unreachable
             (to_string
                t
             )
             error_string
          );
        assert(false)
      );

    let nb_redirect_message =
      Int_distribution.C.get_key_occurrence
        5
        t.icmp_type
    in
    let nb_icmp_rm_packet =
      Batteries.Option.map_default
        (fun icmp_rm_metrics -> icmp_rm_metrics.Icmp_rm_metrics.nb_packets)
        0
        t.icmp_rm_metrics_option
    in
    if nb_icmp_rm_packet <> nb_redirect_message then
      (
        print_endline
          (sprintf
             "Icmp_metrics: verify: inconsistency between number of icmp rm packets (%d) and number of icmp redirect message packets (%d):\n%s\n\n\n%s"
             nb_icmp_rm_packet
             nb_redirect_message
             (to_string
                t
             )
             error_string
          );
        assert(false)
      );


    let nb_time_exceeded =
      Int_distribution.C.get_key_occurrence
        11
        t.icmp_type
    in
    let nb_icmp_te_packet =
      Batteries.Option.map_default
        (fun icmp_te_metrics -> icmp_te_metrics.Icmp_te_metrics.nb_packets)
        0
        t.icmp_te_metrics_option
    in
    if nb_icmp_te_packet <> nb_time_exceeded then
      (
        print_endline
          (sprintf
             "Icmp_metrics: verify: inconsistency between number of icmp te packets (%d) and number of icmp time exceeded packets (%d):\n%s\n\n\n%s"
             nb_icmp_te_packet
             nb_time_exceeded
             (to_string
                t
             )
             error_string
          );
        assert(false)
      );

    Batteries.Option.may
      (Icmp_du_metrics.verify "")
      t.icmp_du_metrics_option;
    Batteries.Option.may
      (Icmp_rm_metrics.verify "")
      t.icmp_rm_metrics_option;
    Batteries.Option.may
      (Icmp_te_metrics.verify "")
      t.icmp_te_metrics_option;
  )

let copy t = 
  new_t
    (Int_distribution.C.copy t.icmp_type)

    (Batteries.Option.map Icmp_du_metrics.copy t.icmp_du_metrics_option)
    (Batteries.Option.map Icmp_rm_metrics.copy t.icmp_rm_metrics_option)
    (Batteries.Option.map Icmp_te_metrics.copy t.icmp_te_metrics_option)

let append
    t1
    t2
    =
  (
    Int_distribution.C.append
      t1.icmp_type
      t2.icmp_type;

    Option_utils.append
      Icmp_du_metrics.append
      (fun icmp_du_metrics_to_append ->
          t1.icmp_du_metrics_option <- Some (Icmp_du_metrics.copy icmp_du_metrics_to_append)
      )
      t1.icmp_du_metrics_option
      t2.icmp_du_metrics_option;

    Option_utils.append
      Icmp_rm_metrics.append
      (fun icmp_rm_metrics_to_append ->
          t1.icmp_rm_metrics_option <- Some (Icmp_rm_metrics.copy icmp_rm_metrics_to_append)
      )
      t1.icmp_rm_metrics_option
      t2.icmp_rm_metrics_option;

    Option_utils.append
      Icmp_te_metrics.append
      (fun icmp_te_metrics_to_append ->
          t1.icmp_te_metrics_option <- Some (Icmp_te_metrics.copy icmp_te_metrics_to_append)
      )
      t1.icmp_te_metrics_option
      t2.icmp_te_metrics_option;
  )

let fusion
    t1
    t2
  =
  (
    let icmp_type =
      Int_distribution.C.fusion
        t1.icmp_type
        t2.icmp_type
    in

    let icmp_du_metrics_option =
      Option_utils.fusion
        Icmp_du_metrics.fusion
        t1.icmp_du_metrics_option
        t2.icmp_du_metrics_option
    in
    let icmp_rm_metrics_option =
      Option_utils.fusion
        Icmp_rm_metrics.fusion
        t1.icmp_rm_metrics_option
        t2.icmp_rm_metrics_option
    in
    let icmp_te_metrics_option =
      Option_utils.fusion
        Icmp_te_metrics.fusion
        t1.icmp_te_metrics_option
        t2.icmp_te_metrics_option
    in

    let t =   
      new_t
        icmp_type

        icmp_du_metrics_option
        icmp_rm_metrics_option
        icmp_te_metrics_option
    in

    t
  )

let update_icmp_data_for_metrics
    t
    icmp_data_for_metrics
  =
  (
    ignore(
      match icmp_data_for_metrics with
      | Icmp_data_for_metrics.Echo_request ->
        (
          Int_distribution.C.add_single_occurrence t.icmp_type 8;
        )
      | Icmp_data_for_metrics.Echo_reply ->
        (
          Int_distribution.C.add_single_occurrence t.icmp_type 0;
        )
      | Icmp_data_for_metrics.Destination_unreachable (code, source_address, destination_address, icmp_transport_layer_data_for_metrics) ->
        (
          Int_distribution.C.add_single_occurrence t.icmp_type 3;

          match t.icmp_du_metrics_option with
          | None ->
            (
              let icmp_du_metrics = Icmp_du_metrics.new_single_t () in
              Icmp_du_metrics.update
                icmp_du_metrics
                (code, source_address, destination_address, icmp_transport_layer_data_for_metrics);
              t.icmp_du_metrics_option <- Some icmp_du_metrics
            )
          | Some icmp_du_metrics ->
            Icmp_du_metrics.update
              icmp_du_metrics
              (code, source_address, destination_address, icmp_transport_layer_data_for_metrics);
        )
      | Icmp_data_for_metrics.Timestamp_request ->
        Int_distribution.C.add_single_occurrence t.icmp_type 13
      | Icmp_data_for_metrics.Timestamp_reply ->
        Int_distribution.C.add_single_occurrence t.icmp_type 14
      | Icmp_data_for_metrics.Netmask_request ->
        Int_distribution.C.add_single_occurrence t.icmp_type 17
      | Icmp_data_for_metrics.Netmask_reply ->
        Int_distribution.C.add_single_occurrence t.icmp_type 18
      | Icmp_data_for_metrics.Source_quench ->
        Int_distribution.C.add_single_occurrence t.icmp_type 4
      | Icmp_data_for_metrics.Redirect_message (code, source_address, destination_address, icmp_transport_layer_data_for_metrics) ->
        (
          Int_distribution.C.add_single_occurrence t.icmp_type 5;

          let icmp_rm_metrics = Icmp_rm_metrics.new_single_t () in
          Icmp_rm_metrics.update
            icmp_rm_metrics 
            (source_address, destination_address, icmp_transport_layer_data_for_metrics);
          let new_icmp_rm_metrics_option =
            Option_utils.fusion
              Icmp_rm_metrics.fusion
              t.icmp_rm_metrics_option
              (Some icmp_rm_metrics)
          in

          t.icmp_rm_metrics_option <- new_icmp_rm_metrics_option;
        )
      | Icmp_data_for_metrics.Router_advertisement ->
        Int_distribution.C.add_single_occurrence t.icmp_type 9
      | Icmp_data_for_metrics.Router_solicitation ->
        Int_distribution.C.add_single_occurrence t.icmp_type 10
      | Icmp_data_for_metrics.Time_exceeded (code, source_address, destination_address, icmp_transport_layer_data_for_metrics) ->
        (
          Int_distribution.C.add_single_occurrence t.icmp_type 11;

          let icmp_te_metrics = Icmp_te_metrics.new_single_t () in
          Icmp_te_metrics.update
            icmp_te_metrics 
            (code, source_address, destination_address, icmp_transport_layer_data_for_metrics);
          let new_icmp_te_metrics_option =
            Option_utils.fusion
              Icmp_te_metrics.fusion
              t.icmp_te_metrics_option
              (Some icmp_te_metrics)
          in

          t.icmp_te_metrics_option <- new_icmp_te_metrics_option;
        )
      | Icmp_data_for_metrics.Other ->
        Int_distribution.C.add_single_occurrence t.icmp_type (-1)
    );
  )
