
open Printf

open Sexplib.Std
open Bin_prot.Std

open Set_ext_instantiations

open Key_occurrence_distribution_instantiations
open Network_traffic_metric_instantiations

let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Network_traffic_attributes]: %s@." s)
      else
        ignore
    )
    fmt

type t =
  {
    map : float Core.Std.Int.Map.t;
  }
with compare, sexp, bin_io

let new_t
    map
  =
  {
    map;
  }

let to_string t =
  let l = Core.Std.Int.Map.to_alist t.map in

  List_ext.to_string
    ~sep: " ; "
    (fun (key, attribute) -> string_of_float attribute)
    l
    
(* let to_string to_string_mode t = *)
  (* match to_string_mode with *)
  (* | To_string_mode.Command -> *)
  (*   Core_maps.to_string_core_int_map *)
  (*     ~sep_element: " ; " *)
  (*     ~sep_key_value: "" *)
  (*     (fun key -> "") *)
  (*     (fun attribute -> string_of_float attribute) *)
  (*     t.map *)
  (* | To_string_mode.Simple -> *)
  (*   Core_maps.to_string_core_int_map *)
  (*     ~sep_element: " ; " *)
  (*     ~sep_key_value: "" *)
  (*     (fun key -> "") *)
  (*     (fun attribute -> string_of_float attribute) *)
  (*     t.map *)
  (* | To_string_mode.Normal -> *)
  (*   "Network_traffic_attributes: " *)
  (*   ^ Core_maps.to_string_core_int_map *)
  (*     ~sep_element: " ; " *)
  (*     string_of_int *)
  (*     (fun attribute -> string_of_float attribute) *)
  (*     t.map *)

let fusion t1 t2 =
  failwith "Network_traffic_atttributes: fusion cannot USE"

let find_name feature_name_container t name =
  let indice =
    Feature_name_container.find_indice
      feature_name_container
      name
  in
  try
    Core.Std.Int.Map.find_exn t.map indice
  with
  | Not_found ->
    failwith
      (sprintf
         "Network_traffic_attributes: could not find attribute for indice %d:\n%s"
         indice
         (to_string t)
      )

let find_indice t indice = 
  try
    Core.Std.Int.Map.find_exn t.map indice
  with
  | Not_found ->
    failwith
      (sprintf
         "Network_traffic_attributes: could not find attribute for indice %d:\n%s"
         indice
         (to_string t)
      )

let add t_1 t_2 =
  let map =
    Core.Std.Int.Map.merge
      t_1.map
      t_2.map
      (fun ~key: indice element_option ->
        (
          match element_option with
          | `Both (attribute_1, attribute_2) ->
            Some (attribute_1 +. attribute_2)
          | `Left attribute_1 ->
            print_endline
              (sprintf
                 "[Network_traffic_attributes]: attribute for %d absent in (at least) t_1:\n%s\n%s"
                 indice
                 (to_string t_1)
                 (to_string t_2)
              );
            assert(false)
          | `Right attribute_2 ->
            print_endline
              (sprintf
                 "[Network_traffic_attributes]: attribute for %d absent in t_2:\n%s\n%s"
                 indice
                 (to_string t_1)
                 (to_string t_2)
              );
            assert(false)
        )
      )
  in
  
  new_t
    map

let get_result_divide_by_value t value =
  let float_value = float_of_int value in
  
  let map =
    Core.Std.Int.Map.map
      t.map
      (fun attribute -> attribute /. float_value)
  in
  
  new_t
    map


(* TODO: change nb_icmp_du_echo_request/nb_icmp_du_echo_reply to
   nb_icmp_du_icmp_echo_request/nb_icmp_du_icmp_echo_reply *)
let to_indice_name_tuple_array _ =
  let list =
    [
      (0, "nb_src_addr") ;
      (1, "nb_dst_addr") ;
      (2, "src_addr_entropy") ;
      (3, "dst_addr_entropy") ;

      (4, "nb_packets") ;
      (5, "nb_bytes") ;

      (10, "nb_zmap");
      (11, "nb_masscan");
      (12, "nb_zmap_masscan");

      (15, "duration") ;
      (16, "start_time");
      (17, "end_time");
      (18, "start_time_delta") ;
      (19, "end_time_delta") ;

      (* ## ICMP *)
      (20, "nb_icmp") ;
      (21, "nb_echo_reply") ;
      (22, "nb_destination_unreachable") ;
      (23, "nb_source_quench") ;
      (24, "nb_redirect_message") ;
      (25, "nb_echo_request") ;
      (26, "nb_time_exceeded") ;
      (27, "nb_timestamp_request") ;
      (28, "nb_timestamp_reply") ;
      (29, "nb_netmask_request") ;
      (30, "nb_netmask_reply") ;

      (* ## Destination unreachable *)
      (40, "nb_network_unreachable") ;
      (41, "nb_host_unreachable") ;
      (42, "nb_protocol_unreachable") ;
      (43, "nb_port_unreachable") ;
      (44, "nb_network_prohibited") ;
      (45, "nb_host_prohibited") ;
      (46, "nb_communication_prohibited") ;

      (47, "nb_icmp_src_addr") ;
      (48, "nb_icmp_dst_addr") ;

      (* ## Destination unreachable embedded packets *)
      (50, "nb_icmp_du_src_addr") ;
      (51, "nb_icmp_du_dst_addr") ;
      (52, "src_addr_icmp_du_dst_addr_similarity") ;
      (53, "dst_addr_icmp_du_src_addr_similarity") ;
      (55, "nb_icmp_du_icmp") ;
      (56, "nb_icmp_du_tcp") ;
      (57, "nb_icmp_du_udp") ;
      (58, "nb_icmp_du_echo_request") ;
      (59, "nb_icmp_du_echo_reply") ;

      (* ## Redirect message embedded packets *)
      (60, "nb_icmp_rm_src_addr") ;
      (61, "nb_icmp_rm_dst_addr") ;
      (65, "nb_icmp_rm_icmp") ;
      (66, "nb_icmp_rm_tcp") ;
      (67, "nb_icmp_rm_udp") ;
      (68, "nb_icmp_rm_icmp_echo_request") ;
      (69, "nb_icmp_rm_icmp_echo_reply") ;

      (* ## Time exceeded embedded packets *)
      (70, "nb_icmp_te_src_addr") ;
      (71, "nb_icmp_te_dst_addr") ;
      (75, "nb_icmp_te_icmp") ;
      (76, "nb_icmp_te_tcp") ;
      (77, "nb_icmp_te_udp") ;
      (78, "nb_icmp_te_icmp_echo_request") ;
      (79, "nb_icmp_te_icmp_echo_reply") ;

      (* ## Ports *)
      (80, "nb_src_port") ;
      (81, "nb_dst_port") ;

      (82, "prop_src_port_system") ;
      (83, "prop_dst_port_system") ;

      (84, "biggest_src_port_over_every_other_port") ;
      (85, "biggest_dst_port_over_every_other_port") ;

      (* ## TCP *)
      (90, "nb_tcp") ;
      (91, "nb_urg") ;
      (92, "nb_ack") ;
      (93, "nb_psh") ;
      (94, "nb_rst") ;
      (95, "nb_syn") ;
      (96, "nb_fin") ;

      (* ## UDP *)
      (100, "nb_udp") ;
      (101, "udp_data_length_mean") ;
      (102, "udp_data_length_med") ;
      (103, "udp_data_length_std") ;
      (104, "udp_data_length_idr") ;
      (105, "udp_data_length_mad") ;
      
      (108, "nb_ipv6_in_ipv4") ;

      (109, "nb_gre_in_ipv4") ;
      
      (115, "nb_transport_protocol") ;
    ]
  in

  let _int_map = Map_ext_instantiations.Int_map.of_list_no_dup list in
  let _string_map = Map_ext_instantiations.String_map.of_list_no_dup (List.map (fun (indice, name) -> (name, indice)) list) in

  Array.of_list list

let generate_feature_name_container
    _
  =
  let indice_name_tuple_array = to_indice_name_tuple_array () in

  Feature_name_container.of_indice_name_tuple_array
    indice_name_tuple_array

(* do not check value *)
let generate_unbounded_attribute_indice_name_list () =
  [
    (2, "src_addr_entropy") ;
    (3, "dst_addr_entropy") ;
  ]

(* will check 0. <= value *)
let generate_greater_than_zero_attribute_indice_name_list () =
  [
    (0, "nb_src_addr") ;
    (1, "nb_dst_addr") ;

    (4, "nb_packets") ;
    (5, "nb_bytes") ;

    (10, "nb_zmap") ;
    (11, "nb_masscan") ;
    (12, "nb_zmap_masscan") ;

    (15, "duration") ;
    (16, "start_time") ;
    (17, "end_time") ;
    (18, "start_time_delta") ;
    (19, "end_time_delta") ;

    (* ## ICMP *)
    (20, "nb_icmp") ;
    (21, "nb_echo_reply") ;
    (22, "nb_destination_unreachable") ;
    (23, "nb_source_quench") ;
    (24, "nb_redirect_message") ;
    (25, "nb_echo_request") ;
    (26, "nb_time_exceeded") ;
    (27, "nb_timestamp_request") ;
    (28, "nb_timestamp_reply") ;
    (29, "nb_netmask_request") ;
    (30, "nb_netmask_reply") ;

    (* ## Destination unreachable *)
    (40, "nb_network_unreachable") ;
    (41, "nb_host_unreachable") ;
    (42, "nb_protocol_unreachable") ;
    (43, "nb_port_unreachable") ;
    (44, "nb_network_prohibited") ;
    (45, "nb_host_prohibited") ;
    (46, "nb_communication_prohibited") ;

    (47, "nb_icmp_src_addr") ;
    (48, "nb_icmp_dst_addr") ;

    (* ## Destination unreachable embedded packets *)
    (50, "nb_icmp_du_src_addr") ;
    (51, "nb_icmp_du_dst_addr") ;

    (55, "nb_icmp_du_icmp") ;
    (56, "nb_icmp_du_tcp") ;
    (57, "nb_icmp_du_udp") ;
    (58, "nb_icmp_du_echo_request") ;
    (59, "nb_icmp_du_echo_reply") ;

    (* ## Redirect message embedded packets *)
    (60, "nb_icmp_rm_src_addr") ;
    (61, "nb_icmp_rm_dst_addr") ;
    (65, "nb_icmp_rm_icmp") ;
    (66, "nb_icmp_rm_tcp") ;
    (67, "nb_icmp_rm_udp") ;
    (68, "nb_icmp_rm_icmp_echo_request") ;
    (69, "nb_icmp_rm_icmp_echo_reply") ;

    (* ## Time exceeded embedded packets *)
    (70, "nb_icmp_te_src_addr") ;
    (71, "nb_icmp_te_dst_addr") ;
    (75, "nb_icmp_te_icmp") ;
    (76, "nb_icmp_te_tcp") ;
    (77, "nb_icmp_te_udp") ;
    (78, "nb_icmp_te_icmp_echo_request") ;
    (79, "nb_icmp_te_icmp_echo_reply") ;

    (* ## Ports *)
    (80, "nb_src_port") ;
    (81, "nb_dst_port") ;

    (84, "biggest_src_port_over_every_other_port") ;
    (85, "biggest_dst_port_over_every_other_port") ;

    (* ## TCP *)
    (90, "nb_tcp") ;
    (91, "nb_urg") ;
    (92, "nb_ack") ;
    (93, "nb_psh") ;
    (94, "nb_rst") ;
    (95, "nb_syn") ;
    (96, "nb_fin") ;

    (* ## UDP *)
    (100, "nb_udp") ;
    (101, "udp_data_length_mean") ;
    (102, "udp_data_length_med") ;
    (103, "udp_data_length_std") ;
    (104, "udp_data_length_idr") ;
    (105, "udp_data_length_mad") ;

    (108, "nb_ipv6_in_ipv4") ;

    (109, "nb_gre_in_ipv4") ;

    (115, "nb_transport_protocol") ;
  ]

(* will check 0. <= value <= 1.0 *)
let generate_ratio_attribute_indice_name_list () =
  [
    (52, "src_addr_icmp_du_dst_addr_similarity") ;
    (53, "dst_addr_icmp_du_src_addr_similarity") ;

    (82, "prop_src_port_system") ;
    (83, "prop_dst_port_system") ;
  ]


let verify error_string t =
  (
    let unbounded_attribute_indice_name_list =
      generate_unbounded_attribute_indice_name_list
        ()
    in
    let greater_than_zero_attribute_indice_name_list =
      generate_greater_than_zero_attribute_indice_name_list
        ()
    in
    let ratio_attribute_indice_name_tuple_list =
      generate_ratio_attribute_indice_name_list
        ()
    in

    let indice_name_tuple_list = Array.to_list (to_indice_name_tuple_array ()) in
    let indice_int_set = 
      Int_set.of_list
        (List.map
           fst
           indice_name_tuple_list
        )
    in

    let unbounded_attribute_indice_int_set =
      Int_set.of_list
        (List.map
           fst
           unbounded_attribute_indice_name_list
        )
    in
    let greater_than_zero_attribute_indice_int_set =
      Int_set.of_list
        (List.map
           fst
           greater_than_zero_attribute_indice_name_list
        )
    in
    let ratio_attribute_indice_int_set =
      Int_set.of_list
        (List.map
           fst
           ratio_attribute_indice_name_tuple_list
        )
    in

    let inter_greater_than_zero_ratio = 
      Int_set.inter unbounded_attribute_indice_int_set 
        (Int_set.inter
           greater_than_zero_attribute_indice_int_set
           ratio_attribute_indice_int_set
        )
    in
    let union_all =
      Int_set.union unbounded_attribute_indice_int_set 
        (Int_set.union
           greater_than_zero_attribute_indice_int_set
           ratio_attribute_indice_int_set
        )
    in
    assert(Int_set.cardinal inter_greater_than_zero_ratio = 0);

    if Int_set.cardinal indice_int_set <> Int_set.cardinal union_all then
      (
        let inter = Int_set.inter indice_int_set union_all in
        let union = Int_set.union indice_int_set union_all in
        let diff = Int_set.diff union inter in

        print_endline
          (sprintf 
             "Network_traffic_attributes: verify: inconsistency between attributes present and reference:\nt:\n%s\nreference\n:%s\ninter:\n%s\nunion:\n%s" 
             (Int_set.to_string indice_int_set)
             (Int_set.to_string union_all)
             (Int_set.to_string inter)
             (Int_set.to_string union)
          );

        print_endline
          (sprintf 
             "Network_traffic_attributes: verify: diff:\n%s" 
             (Int_set.to_string diff)
          );
        assert(false)
      );

    Int_set.iter
      (fun indice ->
         let value =
           find_indice
             t
             indice
         in

         if value < 0. then
           ( 
             let feature_name_container = generate_feature_name_container () in
             let attribute_name = Feature_name_container.find_name feature_name_container indice in

             print_endline
               (sprintf
                  "Network_attributes: verify: %d - %s = %f less than 0 for:\n%s"
                  indice
                  attribute_name
                  value
                  error_string
               );
             assert(false)
           );
      )
      greater_than_zero_attribute_indice_int_set;

    Int_set.iter
      (fun indice ->
         let value =
           find_indice
             t
             indice
         in

         if value < 0. || value > 1. then
           (
             let feature_name_container = generate_feature_name_container () in
             let attribute_name = Feature_name_container.find_name feature_name_container indice in

             print_endline
               (sprintf
                  "Network_traffic_attributes: verify: %d - %s = %f less than 0 or more than 1 for:\n%s"
                  indice
                  attribute_name
                  value
                  error_string
               );
             assert(false)
           );
      )
      ratio_attribute_indice_int_set;
  )

let int_ratio
    int_1
    int_2
    =
  match int_2 with
  | 0 -> 0.
  | _ -> (float_of_int int_1) /. (float_of_int int_2)

let ratio_number_of_elements_int32
    distribution_1
    distribution_2
    =
  int_ratio
    (Int32_distribution.C.length distribution_1)
    (Int32_distribution.C.length distribution_2)

let ratio_number_of_elements
    distribution_1
    distribution_2
    =
  int_ratio
    (Ipaddr_distribution.C.length distribution_1)
    (Ipaddr_distribution.C.length distribution_2)

let float_ratio
    float_1
    float_2
    =
  match float_2 with
  | 0. -> 0.
  | _ -> float_1 /. float_2

let of_trace_statistics_detailed_metrics
    trace_statistics
    detailed_metrics
  =
  (
    Detailed_metrics.verify
      (Detailed_metrics.to_string 
         detailed_metrics)
      detailed_metrics;

    let nb_packets = detailed_metrics.Detailed_metrics.nb_packets in
    let nb_bytes = detailed_metrics.Detailed_metrics.nb_bytes in
    let nb_packets_float = float_of_int nb_packets in
    let nb_bytes_float = float_of_int nb_bytes in



    let nb_zmap =
      Packet_fingerprint_distribution.C.get_key_occurrence
        Packet_fingerprint.Zmap
        detailed_metrics.Detailed_metrics.packet_fingerprint_distribution
    in
    let nb_masscan =
      Packet_fingerprint_distribution.C.get_key_occurrence
        Packet_fingerprint.Masscan
        detailed_metrics.Detailed_metrics.packet_fingerprint_distribution
    in
    let nb_zmap_masscan =
      Packet_fingerprint_distribution.C.get_key_occurrence
        Packet_fingerprint.Zmap_masscan
        detailed_metrics.Detailed_metrics.packet_fingerprint_distribution
    in
    let nb_zmap_float =
      float_of_int
        nb_zmap
    in
    let nb_masscan_float =
      float_of_int
        nb_masscan
    in
    let nb_zmap_masscan_float =
      float_of_int
        nb_zmap_masscan
    in



    let span_start =
      Core.Span.create 
        ~sec: detailed_metrics.Detailed_metrics.timestamp_sec_start
        ~us: detailed_metrics.Detailed_metrics.timestamp_usec_start
        () 
    in
    let core_time_start = Core.Time.add Core.Time.epoch span_start in
    let span_end =
      Core.Span.create 
        ~sec: detailed_metrics.Detailed_metrics.timestamp_sec_end
        ~us: detailed_metrics.Detailed_metrics.timestamp_usec_end
        () 
    in
    let core_time_end = Core.Time.add Core.Time.epoch span_end in

    let anomaly_length_span = Core.Span.abs (Core.Time.diff core_time_start core_time_end) in
    let duration = Core.Span.to_sec anomaly_length_span in

    let start_time =
      if nb_packets = 0 then
        trace_statistics.Trace_statistics.start_time_of_capture
      else
        (float_of_int detailed_metrics.Detailed_metrics.timestamp_sec_start)
        +.
        (float_of_int detailed_metrics.Detailed_metrics.timestamp_usec_start *. 10.**(-6.))
    in
    let end_time =
      if nb_packets = 0 then
        trace_statistics.Trace_statistics.start_time_of_capture
      else
        (float_of_int detailed_metrics.Detailed_metrics.timestamp_sec_end)
        +.
        (float_of_int detailed_metrics.Detailed_metrics.timestamp_usec_end *. 10.**(-6.))
    in

    let start_time_delta = start_time -. trace_statistics.Trace_statistics.start_time_of_capture in
    let end_time_delta = trace_statistics.Trace_statistics.end_time_of_capture -. end_time in




    let nb_src_addr = float_of_int (Ipaddr_distribution.C.length detailed_metrics.Detailed_metrics.src_addr) in
    let nb_dst_addr = float_of_int (Ipaddr_distribution.C.length detailed_metrics.Detailed_metrics.dst_addr) in

    let src_addr_shannon_entropy = Ipaddr_distribution.C.shannon_entropy detailed_metrics.Detailed_metrics.src_addr in
    let dst_addr_shannon_entropy = Ipaddr_distribution.C.shannon_entropy detailed_metrics.Detailed_metrics.dst_addr in

    let nb_transport_protocol =
      float_of_int
        (
          Transport_protocol_for_metrics_distribution.C.length
            detailed_metrics.Detailed_metrics.transport_protocol_for_metrics_distribution
        )
    in

    (* #### ICMP  *)

    let (nb_icmp, 
         nb_echo_reply, nb_destination_unreachable, nb_source_quench, nb_redirect_message, nb_echo_request, 
         nb_time_exceeded, nb_timestamp_request, nb_timestamp_reply, nb_netmask_request, nb_netmask_reply) =
      Batteries.Option.map_default
        (fun icmp_metrics ->
           let nb_icmp = float_of_int (Icmp_metrics.get_nb_packets icmp_metrics) in
           let nb_echo_reply =
             float_of_int 
               (Int_distribution.C.get_key_occurrence
                  0
                  icmp_metrics.Icmp_metrics.icmp_type)
           in
           let nb_destination_unreachable =
             float_of_int 
               (Int_distribution.C.get_key_occurrence
                  3
                  icmp_metrics.Icmp_metrics.icmp_type)
           in
           let nb_source_quench =
             float_of_int 
               (Int_distribution.C.get_key_occurrence
                  4
                  icmp_metrics.Icmp_metrics.icmp_type)
           in
           let nb_redirect_message =
             float_of_int 
               (Int_distribution.C.get_key_occurrence
                  5
                  icmp_metrics.Icmp_metrics.icmp_type)
           in
           let nb_echo_request =
             float_of_int 
               (Int_distribution.C.get_key_occurrence
                  8
                  icmp_metrics.Icmp_metrics.icmp_type)
           in
           let nb_time_exceeded =
             float_of_int 
               (Int_distribution.C.get_key_occurrence
                  11
                  icmp_metrics.Icmp_metrics.icmp_type)
           in
           let nb_timestamp_request =
             float_of_int 
               (Int_distribution.C.get_key_occurrence
                  13
                  icmp_metrics.Icmp_metrics.icmp_type)
           in
           let nb_timestamp_reply =
             float_of_int
               (Int_distribution.C.get_key_occurrence
                  14
                  icmp_metrics.Icmp_metrics.icmp_type)
           in
           let nb_netmask_request =
             float_of_int 
               (Int_distribution.C.get_key_occurrence
                  17
                  icmp_metrics.Icmp_metrics.icmp_type)
           in
           let nb_netmask_reply =
             float_of_int 
               (Int_distribution.C.get_key_occurrence
                  18
                  icmp_metrics.Icmp_metrics.icmp_type)
           in

           (nb_icmp, 
            nb_echo_reply, nb_destination_unreachable, nb_source_quench, nb_redirect_message, nb_echo_request, 
            nb_time_exceeded, nb_timestamp_request, nb_timestamp_reply, nb_netmask_request, nb_netmask_reply)
        )
        (0.,
         0., 0., 0., 0., 0.,
         0., 0., 0., 0., 0.)
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in

    (* ### Destination unreachable code *)

    let nb_network_unreachable =
      Batteries.Option.map_default
        (fun icmp_metrics ->
           Batteries.Option.map_default
             (fun icmp_du_metrics ->
                float_of_int
                  (Int_distribution.C.get_key_occurrence
                     0 
                     icmp_du_metrics.Icmp_du_metrics.code)
             )
             0.
             icmp_metrics.Icmp_metrics.icmp_du_metrics_option
        )
        0.
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in
    let nb_host_unreachable =
      Batteries.Option.map_default
        (fun icmp_metrics ->
           Batteries.Option.map_default
             (fun icmp_du_metrics ->
                float_of_int
                  (Int_distribution.C.get_key_occurrence
                     1
                     icmp_du_metrics.Icmp_du_metrics.code)
             )
             0.
             icmp_metrics.Icmp_metrics.icmp_du_metrics_option
        )
        0.
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in
    let nb_protocol_unreachable =
      Batteries.Option.map_default
        (fun icmp_metrics ->
           Batteries.Option.map_default
             (fun icmp_du_metrics ->
                float_of_int
                  (Int_distribution.C.get_key_occurrence
                     2
                     icmp_du_metrics.Icmp_du_metrics.code)
             )
             0.
             icmp_metrics.Icmp_metrics.icmp_du_metrics_option
        )
        0.
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in
    let nb_port_unreachable =
      Batteries.Option.map_default
        (fun icmp_metrics ->
           Batteries.Option.map_default
             (fun icmp_du_metrics ->
                float_of_int
                  (Int_distribution.C.get_key_occurrence
                     3
                     icmp_du_metrics.Icmp_du_metrics.code)
             )
             0.
             icmp_metrics.Icmp_metrics.icmp_du_metrics_option
        )
        0.
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in
    let nb_network_prohibited =
      Batteries.Option.map_default
        (fun icmp_metrics ->
           Batteries.Option.map_default
             (fun icmp_du_metrics ->
                float_of_int
                  (Int_distribution.C.get_key_occurrence
                     9
                     icmp_du_metrics.Icmp_du_metrics.code)
             )
             0.
             icmp_metrics.Icmp_metrics.icmp_du_metrics_option
        )
        0.
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in        
    let nb_host_prohibited =
      Batteries.Option.map_default
        (fun icmp_metrics ->
           Batteries.Option.map_default
             (fun icmp_du_metrics ->
                float_of_int
                  (Int_distribution.C.get_key_occurrence
                     10
                     icmp_du_metrics.Icmp_du_metrics.code)
             )
             0.
             icmp_metrics.Icmp_metrics.icmp_du_metrics_option
        )
        0.
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in        
    let nb_communication_prohibited =
      Batteries.Option.map_default
        (fun icmp_metrics ->
           Batteries.Option.map_default
             (fun icmp_du_metrics ->
                float_of_int
                  (Int_distribution.C.get_key_occurrence
                     13
                     icmp_du_metrics.Icmp_du_metrics.code)
             )
             0.
             icmp_metrics.Icmp_metrics.icmp_du_metrics_option
        )
        0.
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in        

    (* ### ICMP DU/RM/TE packets data *)

    let (icmp_src_addr, icmp_dst_addr) =
      Batteries.Option.map_default
        (fun icmp_metrics ->

           let (icmp_du_src_addr, icmp_du_dst_addr) =
             Batteries.Option.map_default
               (fun icmp_du_metrics ->
                  let icmp_encapsulated_metrics = icmp_du_metrics.Icmp_du_metrics.icmp_encapsulated_metrics in

                  (icmp_encapsulated_metrics.Icmp_encapsulated_metrics.src_addr,
                   icmp_encapsulated_metrics.Icmp_encapsulated_metrics.dst_addr)
               )
               (Ipaddr_distribution.C.new_empty_t (),
                Ipaddr_distribution.C.new_empty_t ())
               icmp_metrics.Icmp_metrics.icmp_du_metrics_option
           in

           let (icmp_rm_src_addr, icmp_rm_dst_addr) =
             Batteries.Option.map_default
               (fun icmp_rm_metrics ->
                  let icmp_encapsulated_metrics = icmp_rm_metrics.Icmp_rm_metrics.icmp_encapsulated_metrics in

                  (icmp_encapsulated_metrics.Icmp_encapsulated_metrics.src_addr,
                   icmp_encapsulated_metrics.Icmp_encapsulated_metrics.dst_addr)
               )
               (Ipaddr_distribution.C.new_empty_t (),
                Ipaddr_distribution.C.new_empty_t ())
               icmp_metrics.Icmp_metrics.icmp_rm_metrics_option
           in

           let (icmp_te_src_addr, icmp_te_dst_addr) =
             Batteries.Option.map_default
               (fun icmp_te_metrics ->
                  let icmp_encapsulated_metrics = icmp_te_metrics.Icmp_te_metrics.icmp_encapsulated_metrics in

                  (icmp_encapsulated_metrics.Icmp_encapsulated_metrics.src_addr,
                   icmp_encapsulated_metrics.Icmp_encapsulated_metrics.dst_addr)
               )
               (Ipaddr_distribution.C.new_empty_t (),
                Ipaddr_distribution.C.new_empty_t ())
               icmp_metrics.Icmp_metrics.icmp_te_metrics_option
           in

           (
             (Ipaddr_distribution.C.fusion
                (Ipaddr_distribution.C.fusion
                   icmp_du_src_addr
                   icmp_rm_src_addr
                )
                icmp_te_src_addr
             )
             ,
             (Ipaddr_distribution.C.fusion
                (Ipaddr_distribution.C.fusion
                   icmp_du_dst_addr
                   icmp_rm_dst_addr
                )
                icmp_te_dst_addr
             )
           )
        )
        (Ipaddr_distribution.C.new_empty_t (),
         Ipaddr_distribution.C.new_empty_t ())
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in
    let nb_icmp_src_addr =
      float_of_int
        (Ipaddr_distribution.C.length icmp_src_addr)
    in
    let nb_icmp_dst_addr =
      float_of_int
        (Ipaddr_distribution.C.length icmp_dst_addr)
    in

    (* ### ICMP DU packets data *)
    let (nb_icmp_du_src_addr, nb_icmp_du_dst_addr,
         src_addr_icmp_du_dst_addr_similarity, dst_addr_icmp_du_src_addr_similarity,
         nb_icmp_du_icmp, nb_icmp_du_icmp_echo_request, nb_icmp_du_icmp_echo_reply, nb_icmp_du_tcp, nb_icmp_du_udp) =
      Batteries.Option.map_default
        (fun icmp_metrics ->
           Batteries.Option.map_default
             (fun icmp_du_metrics ->
                let icmp_encapsulated_metrics = icmp_du_metrics.Icmp_du_metrics.icmp_encapsulated_metrics in

                let nb_icmp_du_src_addr =
                  float_of_int
                    (Ipaddr_distribution.C.length
                       icmp_encapsulated_metrics.Icmp_encapsulated_metrics.src_addr)
                in
                let nb_icmp_du_dst_addr =
                  float_of_int
                    (Ipaddr_distribution.C.length
                       icmp_encapsulated_metrics.Icmp_encapsulated_metrics.dst_addr)
                in

                let src_addr_icmp_du_dst_addr_similarity =
                  Ipaddr_distribution.C.jaccard
                    detailed_metrics.Detailed_metrics.src_addr
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.dst_addr
                in
                let dst_addr_icmp_du_src_addr_similarity =
                  Ipaddr_distribution.C.jaccard
                    detailed_metrics.Detailed_metrics.dst_addr
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.src_addr
                in

                let (nb_icmp_du_icmp, nb_icmp_du_icmp_echo_request, nb_icmp_du_icmp_echo_reply) =
                  Batteries.Option.map_default
                    (fun icmp_icmp_metrics ->
                       let nb_icmp_du_icmp =
                         float_of_int icmp_icmp_metrics.Icmp_icmp_metrics.nb_icmp_icmp_packets
                       in

                       let nb_icmp_du_icmp_echo_request =
                         float_of_int
                           (Int_distribution.C.get_key_occurrence
                              8
                              icmp_icmp_metrics.Icmp_icmp_metrics.icmp_type)
                       in

                       let nb_icmp_du_icmp_echo_reply =
                         float_of_int
                           (Int_distribution.C.get_key_occurrence
                              0
                              icmp_icmp_metrics.Icmp_icmp_metrics.icmp_type)
                       in

                       (nb_icmp_du_icmp, nb_icmp_du_icmp_echo_request, nb_icmp_du_icmp_echo_reply)
                    )
                    (0., 0., 0.)
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_icmp_metrics_option
                in

                let nb_icmp_du_tcp =
                  Batteries.Option.map_default
                    (fun icmp_tcp_metrics ->
                       float_of_int icmp_tcp_metrics.Icmp_tcp_metrics.nb_icmp_tcp_packets
                    )
                    0.
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_tcp_metrics_option
                in

                let nb_icmp_du_udp =
                  Batteries.Option.map_default
                    (fun icmp_udp_metrics ->
                       float_of_int icmp_udp_metrics.Udp_metrics.nb_udp_packets
                    )
                    0.
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_udp_metrics_option
                in

                (nb_icmp_du_src_addr, nb_icmp_du_dst_addr,
                 src_addr_icmp_du_dst_addr_similarity, dst_addr_icmp_du_src_addr_similarity,
                 nb_icmp_du_icmp, nb_icmp_du_icmp_echo_request, nb_icmp_du_icmp_echo_reply, nb_icmp_du_tcp, nb_icmp_du_udp)
             )
             (0., 0., 0., 0., 0., 0., 0., 0., 0.)
             icmp_metrics.Icmp_metrics.icmp_du_metrics_option
        )
        (0., 0., 0., 0., 0., 0., 0., 0., 0.)
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in

    (* ### ICMP RM packets data *)
    let (nb_icmp_rm_src_addr, nb_icmp_rm_dst_addr,
         src_addr_icmp_rm_dst_addr_similarity, dst_addr_icmp_rm_src_addr_similarity,
         nb_icmp_rm_icmp, nb_icmp_rm_icmp_echo_request, nb_icmp_rm_icmp_echo_reply, nb_icmp_rm_tcp, nb_icmp_rm_udp) =
      Batteries.Option.map_default
        (fun icmp_metrics ->
           Batteries.Option.map_default
             (fun icmp_rm_metrics ->
                let icmp_encapsulated_metrics = icmp_rm_metrics.Icmp_rm_metrics.icmp_encapsulated_metrics in

                let nb_icmp_rm_src_addr =
                  float_of_int
                    (Ipaddr_distribution.C.length
                       icmp_encapsulated_metrics.Icmp_encapsulated_metrics.src_addr)
                in
                let nb_icmp_rm_dst_addr =
                  float_of_int
                    (Ipaddr_distribution.C.length
                       icmp_encapsulated_metrics.Icmp_encapsulated_metrics.dst_addr)
                in

                let src_addr_icmp_rm_dst_addr_similarity =
                  Ipaddr_distribution.C.jaccard
                    detailed_metrics.Detailed_metrics.src_addr
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.dst_addr
                in
                let dst_addr_icmp_rm_src_addr_similarity =
                  Ipaddr_distribution.C.jaccard
                    detailed_metrics.Detailed_metrics.dst_addr
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.src_addr
                in

                let (nb_icmp_rm_icmp, nb_icmp_rm_icmp_echo_request, nb_icmp_rm_icmp_echo_reply) =
                  Batteries.Option.map_default
                    (fun icmp_icmp_metrics ->
                       let nb_icmp_rm_icmp =
                         float_of_int icmp_icmp_metrics.Icmp_icmp_metrics.nb_icmp_icmp_packets
                       in

                       let nb_icmp_rm_icmp_echo_request =
                         float_of_int
                           (Int_distribution.C.get_key_occurrence
                              8
                              icmp_icmp_metrics.Icmp_icmp_metrics.icmp_type)
                       in

                       let nb_icmp_rm_icmp_echo_reply =
                         float_of_int
                           (Int_distribution.C.get_key_occurrence
                              0
                              icmp_icmp_metrics.Icmp_icmp_metrics.icmp_type)
                       in

                       (nb_icmp_rm_icmp, nb_icmp_rm_icmp_echo_request, nb_icmp_rm_icmp_echo_reply)
                    )
                    (0., 0., 0.)
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_icmp_metrics_option
                in

                let nb_icmp_rm_tcp =
                  Batteries.Option.map_default
                    (fun icmp_tcp_metrics ->
                       float_of_int icmp_tcp_metrics.Icmp_tcp_metrics.nb_icmp_tcp_packets
                    )
                    0.
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_tcp_metrics_option
                in

                let nb_icmp_rm_udp =
                  Batteries.Option.map_default
                    (fun icmp_udp_metrics ->
                       float_of_int icmp_udp_metrics.Udp_metrics.nb_udp_packets
                    )
                    0.
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_udp_metrics_option
                in

                (nb_icmp_rm_src_addr, nb_icmp_rm_dst_addr,
                 src_addr_icmp_rm_dst_addr_similarity, dst_addr_icmp_rm_src_addr_similarity,
                 nb_icmp_rm_icmp, nb_icmp_rm_icmp_echo_request, nb_icmp_rm_icmp_echo_reply, nb_icmp_rm_tcp, nb_icmp_rm_udp)
             )
             (0., 0., 0., 0., 0., 0., 0., 0., 0.)
             icmp_metrics.Icmp_metrics.icmp_rm_metrics_option
        )
        (0., 0., 0., 0., 0., 0., 0., 0., 0.)
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in

    (* ### ICMP TE packets data *)
    let (nb_icmp_te_src_addr, nb_icmp_te_dst_addr,
         src_addr_icmp_te_dst_addr_similarity, dst_addr_icmp_te_src_addr_similarity,
         nb_icmp_te_icmp, nb_icmp_te_icmp_echo_request, nb_icmp_te_icmp_echo_reply, nb_icmp_te_tcp, nb_icmp_te_udp) =
      Batteries.Option.map_default
        (fun icmp_metrics ->
           Batteries.Option.map_default
             (fun icmp_te_metrics ->
                let icmp_encapsulated_metrics = icmp_te_metrics.Icmp_te_metrics.icmp_encapsulated_metrics in

                let nb_icmp_te_src_addr =
                  float_of_int
                    (Ipaddr_distribution.C.length
                       icmp_encapsulated_metrics.Icmp_encapsulated_metrics.src_addr)
                in
                let nb_icmp_te_dst_addr =
                  float_of_int
                    (Ipaddr_distribution.C.length
                       icmp_encapsulated_metrics.Icmp_encapsulated_metrics.dst_addr)
                in

                let src_addr_icmp_te_dst_addr_similarity =
                  Ipaddr_distribution.C.jaccard
                    detailed_metrics.Detailed_metrics.src_addr
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.dst_addr
                in
                let dst_addr_icmp_te_src_addr_similarity =
                  Ipaddr_distribution.C.jaccard
                    detailed_metrics.Detailed_metrics.dst_addr
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.src_addr
                in

                let (nb_icmp_te_icmp, nb_icmp_te_icmp_echo_request, nb_icmp_te_icmp_echo_reply) =
                  Batteries.Option.map_default
                    (fun icmp_icmp_metrics ->
                       let nb_icmp_te_icmp =
                         float_of_int icmp_icmp_metrics.Icmp_icmp_metrics.nb_icmp_icmp_packets
                       in

                       let nb_icmp_te_icmp_echo_request =
                         float_of_int
                           (Int_distribution.C.get_key_occurrence
                              8
                              icmp_icmp_metrics.Icmp_icmp_metrics.icmp_type)
                       in

                       let nb_icmp_te_icmp_echo_reply =
                         float_of_int
                           (Int_distribution.C.get_key_occurrence
                              0
                              icmp_icmp_metrics.Icmp_icmp_metrics.icmp_type)
                       in

                       (nb_icmp_te_icmp, nb_icmp_te_icmp_echo_request, nb_icmp_te_icmp_echo_reply)
                    )
                    (0., 0., 0.)
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_icmp_metrics_option
                in

                let nb_icmp_te_tcp =
                  Batteries.Option.map_default
                    (fun icmp_tcp_metrics ->
                       float_of_int icmp_tcp_metrics.Icmp_tcp_metrics.nb_icmp_tcp_packets
                    )
                    0.
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_tcp_metrics_option
                in

                let nb_icmp_te_udp =
                  Batteries.Option.map_default
                    (fun icmp_udp_metrics ->
                       float_of_int icmp_udp_metrics.Udp_metrics.nb_udp_packets
                    )
                    0.
                    icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_udp_metrics_option
                in

                (nb_icmp_te_src_addr, nb_icmp_te_dst_addr,
                 src_addr_icmp_te_dst_addr_similarity, dst_addr_icmp_te_src_addr_similarity,
                 nb_icmp_te_icmp, nb_icmp_te_icmp_echo_request, nb_icmp_te_icmp_echo_reply, nb_icmp_te_tcp, nb_icmp_te_udp)
             )
             (0., 0., 0., 0., 0., 0., 0., 0., 0.)
             icmp_metrics.Icmp_metrics.icmp_te_metrics_option
        )
        (0., 0., 0., 0., 0., 0., 0., 0., 0.)
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in

    let src_port, dst_port =
      Detailed_metrics.get_src_dst_port_distribution
        detailed_metrics
    in

    let src_port_system =
      Int_distribution.C.filter
        (fun port _ ->
           port <= 1023
        )
        src_port
    in
    let prop_src_port_system =
      int_ratio
        (Int_distribution.C.get_total_nb_occurrence src_port_system)
        (Int_distribution.C.get_total_nb_occurrence src_port)
    in

    let dst_port_system =
      Int_distribution.C.filter
        (fun port _ ->
           port <= 1023
        )
        dst_port
    in
    let prop_dst_port_system =
      int_ratio
        (Int_distribution.C.get_total_nb_occurrence dst_port_system)
        (Int_distribution.C.get_total_nb_occurrence dst_port)
    in

    (* ## TCP *)

    let (nb_tcp, nb_urg, nb_ack, nb_psh, nb_rst, nb_syn, nb_fin) =
      Batteries.Option.map_default
        (fun tcp_metrics ->
           let nb_tcp = float_of_int tcp_metrics.Tcp_metrics.nb_tcp_packets in
           let nb_urg = float_of_int tcp_metrics.Tcp_metrics.nb_urg_packets in
           let nb_ack = float_of_int tcp_metrics.Tcp_metrics.nb_ack_packets in
           let nb_psh = float_of_int tcp_metrics.Tcp_metrics.nb_psh_packets in
           let nb_rst = float_of_int tcp_metrics.Tcp_metrics.nb_rst_packets in
           let nb_syn = float_of_int tcp_metrics.Tcp_metrics.nb_syn_packets in
           let nb_fin = float_of_int tcp_metrics.Tcp_metrics.nb_fin_packets in

           (nb_tcp, nb_urg, nb_ack, nb_psh, nb_rst, nb_syn, nb_fin)
        )
        (0., 0., 0., 0., 0., 0., 0.)
        detailed_metrics.Detailed_metrics.tcp_metrics_option
    in

    (* ## UDP *)

    let nb_udp =
      Batteries.Option.map_default
        (fun udp_metrics -> float_of_int udp_metrics.Udp_metrics.nb_udp_packets)
        0.
        detailed_metrics.Detailed_metrics.udp_metrics_option
    in

    (* ## Tunnels *)

    let nb_ipv6_in_ipv4 =
      Batteries.Option.map_default
        (fun ipv6_metrics ->
           float_of_int ipv6_metrics.Ipv6_metrics.nb_packets
        )
        0.
        detailed_metrics.Detailed_metrics.ipv6_metrics_option
    in
    let nb_gre_in_ipv4 =
      Batteries.Option.map_default
        (fun gre_metrics ->
           float_of_int gre_metrics.Gre_metrics.nb_packets
        )
        0.
        detailed_metrics.Detailed_metrics.gre_metrics_option
    in

    let biggest_src_port_over_every_other_port =
      (float_of_int (Int_distribution.C.get_biggest_occurrence src_port))
      /.
      (nb_tcp +. nb_udp)
    in
    let biggest_dst_port_over_every_other_port =
      (float_of_int (Int_distribution.C.get_biggest_occurrence dst_port))
      /.
      (nb_tcp +. nb_udp)
    in

    let udp_data_length_mean, udp_data_length_med, udp_data_length_std, udp_data_length_idr, udp_data_length_mad =
      match detailed_metrics.Detailed_metrics.udp_metrics_option with
      | None -> 0., 0., 0., 0., 0.
      | Some udp_metrics ->
        Int_distribution.C.get_mean_med_std_idr_mad_key
          udp_metrics.Udp_metrics.data_length_distribution
    in

    let nb_src_port = float_of_int (Int_distribution.C.length src_port) in

    let nb_dst_port = float_of_int (Int_distribution.C.length dst_port) in    

    let indice_value_tuple_list =
      [
        (0, nb_src_addr) ;
        (1, nb_dst_addr) ;
        (2, src_addr_shannon_entropy);
        (3, dst_addr_shannon_entropy);

        (4, nb_packets_float) ;
        (5, nb_bytes_float) ;

        (10, nb_zmap_float);
        (11, nb_masscan_float);
        (12, nb_zmap_masscan_float);

        (15, duration) ;
        (16, start_time);
        (17, end_time);
        (18, start_time_delta) ;
        (19, end_time_delta) ;

        (* ## ICMP *)
        (20, nb_icmp) ;
        (21, nb_echo_reply) ;
        (22, nb_destination_unreachable) ;
        (23, nb_source_quench) ;
        (24, nb_redirect_message) ;
        (25, nb_echo_request) ;
        (26, nb_time_exceeded) ;
        (27, nb_timestamp_request) ;
        (28, nb_timestamp_reply) ;
        (29, nb_netmask_request) ;
        (30, nb_netmask_reply) ;

        (* ## Destination unreachable *)
        (40, nb_network_unreachable) ;
        (41, nb_host_unreachable) ;
        (42, nb_protocol_unreachable) ;
        (43, nb_port_unreachable) ;
        (44, nb_network_prohibited) ;
        (45, nb_host_prohibited) ;
        (46, nb_communication_prohibited) ;

        (* ## Embedded packets *)
        (47, nb_icmp_src_addr) ;
        (48, nb_icmp_dst_addr) ;

        (* ## Destination unreachable embedded packets *)
        (50, nb_icmp_du_src_addr) ;
        (51, nb_icmp_du_dst_addr) ;

        (52, src_addr_icmp_du_dst_addr_similarity) ;
        (53, dst_addr_icmp_du_src_addr_similarity) ;

        (55, nb_icmp_du_icmp) ;
        (56, nb_icmp_du_tcp) ;
        (57, nb_icmp_du_udp) ;
        (58, nb_icmp_du_icmp_echo_request) ;
        (59, nb_icmp_du_icmp_echo_reply) ;

        (* ## Redirect message embedded packets *)
        (60, nb_icmp_rm_src_addr) ;
        (61, nb_icmp_rm_dst_addr) ;
        (65, nb_icmp_rm_icmp) ;
        (66, nb_icmp_rm_tcp) ;
        (67, nb_icmp_rm_udp) ;
        (68, nb_icmp_rm_icmp_echo_request) ;
        (69, nb_icmp_rm_icmp_echo_reply) ;

        (* ## Time exceeded embedded packets *)
        (70, nb_icmp_te_src_addr) ;
        (71, nb_icmp_te_dst_addr) ;
        (75, nb_icmp_te_icmp) ;
        (76, nb_icmp_te_tcp) ;
        (77, nb_icmp_te_udp) ;
        (78, nb_icmp_te_icmp_echo_request) ;
        (79, nb_icmp_te_icmp_echo_reply) ;

        (* ## Ports *)
        (80, nb_src_port) ;
        (81, nb_dst_port) ;
        (82, prop_src_port_system) ;
        (83, prop_dst_port_system) ;
        (84, biggest_src_port_over_every_other_port) ;
        (85, biggest_dst_port_over_every_other_port) ;

        (* ## TCP *)
        (90, nb_tcp) ;
        (91, nb_urg) ;
        (92, nb_ack) ;
        (93, nb_psh) ;
        (94, nb_rst) ;
        (95, nb_syn) ;
        (96, nb_fin) ;

        (* ## UDP *)
        (100, nb_udp) ;
        (101, udp_data_length_mean) ;
        (102, udp_data_length_med) ;
        (103, udp_data_length_std) ;
        (104, udp_data_length_idr) ;
        (105, udp_data_length_mad) ;

        (108, nb_ipv6_in_ipv4) ;

        (109, nb_gre_in_ipv4) ;

        (115, nb_transport_protocol) ;
      ]
    in
    let map =
      match Core.Std.Int.Map.of_alist indice_value_tuple_list with
      | `Duplicate_key indice ->
        print_endline (sprintf "Network_attributes: of_detailed_metrics: attribute already present with indice %d" indice);
        assert(false)
      | `Ok map -> map
    in

    let t =
      new_t
        map
    in

    verify 
      (
        (Detailed_metrics.to_string detailed_metrics)
        ^
        (to_string t)
      )
      t;

    t
  )

let get_value_of_attribute_id t attribute_id =
  Core.Std.Int.Map.find
    t.map
    attribute_id

let to_feature_array
    t
    =
  let bindings = Core.Std.Int.Map.to_alist t.map in
  let attribute_list = List.map snd bindings in
  
  Array.of_list attribute_list

let merge
    nb_src_ip_address_container
    nb_dst_ip_address_container
    nb_src_port
    nb_dst_port

    t_1
    t_2
  =
  (
    let map =
      Core.Std.Int.Map.merge
        ~f: (fun ~key: key value ->
            match value with
            | `Both (value_1, value_2) ->
              Some
                (
                  match key with
                  (* src_addr_entropy *)
                  | 2 ->
                    0.
                  (* dst_addr_entropy *)
                  | 3 ->
                    0.
                  (* start_time *)
                  | 10 ->
                    min value_1 value_2
                  (* end_time *)
                  | 11 ->
                    max value_1 value_2
                  (* (\* nb_src_port *\) *)
                  (* | 80 -> *)
                  (*   0. *)
                  (* (\* nb_dst_port *\) *)
                  (* | 81 -> *)
                  (*   0. *)
                  (* prop_src_port_system *)
                  | 82 ->
                    0.
                  (* prop_dst_port_system *)
                  | 83 ->
                    0.
                  (* biggest_src_port_over_every_other_port *)
                  | 84 ->
                    0.
                  (* biggest_dst_port_over_every_other_port *)
                  | 85 ->
                    0.
                  | _ ->
                    value_1 +. value_2
                )
            | `Left _ ->
              assert(false)
            | `Right _ ->
              assert(false)
          )
        t_1.map
        t_2.map
    in

    let map =
      Core.Std.Int.Map.remove
        map
        0
    in
    let map =
      Core.Std.Int.Map.add
        map
        0
        nb_src_ip_address_container
    in
    let map =
      Core.Std.Int.Map.remove
        map
        1
    in
    let map =
      Core.Std.Int.Map.add
        map
        1
        nb_dst_ip_address_container
    in


    let map =
      Core.Std.Int.Map.remove
        map
        80
    in
    let map =
      Core.Std.Int.Map.add
        map
        80
        nb_src_port
    in
    let map =
      Core.Std.Int.Map.remove
        map
        81
    in
    let map =
      Core.Std.Int.Map.add
        map
        81
        nb_dst_port
    in
    
    new_t
      map
  )
