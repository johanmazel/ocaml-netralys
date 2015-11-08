
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
        (fun s -> Format.printf "[Network_traffic_values]: %s@." s)
      else
        ignore
    )
    fmt

type t =
  {
    map : Value_variant.t Core.Std.Int.Map.t;
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
    (fun (key, attribute) -> Value_variant.to_string attribute)
    l
    
(* let to_string to_string_mode t = *)
(*   match to_string_mode with *)
(*   | To_string_mode.Command -> *)
(*     Core_maps.to_string_core_int_map *)
(*       ~sep_element: " ; " *)
(*       ~sep_key_value: "" *)
(*       (fun key -> "") *)
(*       (fun value_variant -> Value_variant.to_string value_variant) *)
(*       t.map *)
(*   | To_string_mode.Simple -> *)
(*     Core_maps.to_string_core_int_map *)
(*       ~sep_element: " ; " *)
(*       ~sep_key_value: "" *)
(*       (fun key -> "") *)
(*       (fun value_variant -> Value_variant.to_string value_variant) *)
(*       t.map *)
(*   | To_string_mode.Normal -> *)
(*     "Values: " *)
(*     ^ Core_maps.to_string_int_map *)
(*       ~sep_element: " ; " *)
(*       string_of_int *)
(*       (fun value_variant -> Value_variant.to_string value_variant) *)
(*       t.map *)

let fusion t1 t2 =
  failwith "Network_atttributes: fusion: cannot USE"

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
         "Network_traffic_attributes: could not find value for indice %d:\n%s"
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
         "Network_traffic_attributes: could not find value for indice %d:\n%s"
         indice
         (to_string t)
      )

let to_indice_name_tuple_array _ =
  [|
    (0, "dominant_src_addr");
    (1, "dominant_dst_addr");
    (2, "dominant_src_port");
    (3, "dominant_dst_port");
    (4, "dominant_icmp_src_addr");
    (5, "dominant_icmp_dst_addr");
    (6, "dominant_icmp_src_port");
    (7, "dominant_icmp_dst_port");
  |]

let generate_feature_name_container
    _
  =
  let indice_name_tuple_array = to_indice_name_tuple_array () in

  Feature_name_container.of_indice_name_tuple_array
    indice_name_tuple_array
    
(* TODO: only use Option.map_default once for each option and return monster tuple *)
let of_trace_statistics_detailed_metrics
    trace_statistics
    detailed_metrics
  =
  (
    Detailed_metrics.verify
      (Detailed_metrics.to_string
         detailed_metrics)
      detailed_metrics;




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
                Ipaddr_distribution.C.new_empty_t ()
               )
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
         Ipaddr_distribution.C.new_empty_t ()
        )
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in


    let (icmp_src_port, icmp_dst_port) =
      Batteries.Option.map_default
        (fun icmp_metrics ->

           let (icmp_du_src_port, icmp_du_dst_port) =
             Batteries.Option.map_default
               (fun icmp_du_metrics ->
                  let icmp_encapsulated_metrics = icmp_du_metrics.Icmp_du_metrics.icmp_encapsulated_metrics in

                  let (icmp_du_tcp_src_port, icmp_du_tcp_dst_port) =
                    Batteries.Option.map_default
                      (fun icmp_tcp_metrics ->
                         (icmp_tcp_metrics.Icmp_tcp_metrics.src_port,
                          icmp_tcp_metrics.Icmp_tcp_metrics.dst_port)
                      )
                      (Int_distribution.C.new_empty_t (),
                       Int_distribution.C.new_empty_t ())                    
                      icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_tcp_metrics_option
                  in

                  let (icmp_du_udp_src_port, icmp_du_udp_dst_port) =
                    Batteries.Option.map_default
                      (fun icmp_udp_metrics ->
                         (icmp_udp_metrics.Udp_metrics.src_port,
                          icmp_udp_metrics.Udp_metrics.dst_port)
                      )
                      (Int_distribution.C.new_empty_t (),
                       Int_distribution.C.new_empty_t ())                    
                      icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_udp_metrics_option
                  in

                  (
                    (Int_distribution.C.fusion
                       icmp_du_tcp_src_port
                       icmp_du_udp_src_port
                    )
                    ,
                    (Int_distribution.C.fusion
                       icmp_du_tcp_dst_port
                       icmp_du_udp_dst_port
                    )
                  )
               )
               (Int_distribution.C.new_empty_t (),
                Int_distribution.C.new_empty_t ())
               icmp_metrics.Icmp_metrics.icmp_du_metrics_option
           in

           let (icmp_rm_src_port, icmp_rm_dst_port) =
             Batteries.Option.map_default
               (fun icmp_rm_metrics ->
                  let icmp_encapsulated_metrics = icmp_rm_metrics.Icmp_rm_metrics.icmp_encapsulated_metrics in

                  let (icmp_rm_tcp_src_port, icmp_rm_tcp_dst_port) =
                    Batteries.Option.map_default
                      (fun icmp_tcp_metrics ->
                         (icmp_tcp_metrics.Icmp_tcp_metrics.src_port,
                          icmp_tcp_metrics.Icmp_tcp_metrics.dst_port)
                      )
                      (Int_distribution.C.new_empty_t (),
                       Int_distribution.C.new_empty_t ())                    
                      icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_tcp_metrics_option
                  in

                  let (icmp_rm_udp_src_port, icmp_rm_udp_dst_port) =
                    Batteries.Option.map_default
                      (fun icmp_udp_metrics ->
                         (icmp_udp_metrics.Udp_metrics.src_port,
                          icmp_udp_metrics.Udp_metrics.dst_port)
                      )
                      (Int_distribution.C.new_empty_t (),
                       Int_distribution.C.new_empty_t ())                    
                      icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_udp_metrics_option
                  in

                  (
                    (Int_distribution.C.fusion
                       icmp_rm_tcp_src_port
                       icmp_rm_udp_src_port
                    )
                    ,
                    (Int_distribution.C.fusion
                       icmp_rm_tcp_dst_port
                       icmp_rm_udp_dst_port
                    )
                  )
               )
               (Int_distribution.C.new_empty_t (),
                Int_distribution.C.new_empty_t ())
               icmp_metrics.Icmp_metrics.icmp_rm_metrics_option
           in

           let (icmp_te_src_port, icmp_te_dst_port) =
             Batteries.Option.map_default
               (fun icmp_te_metrics ->
                  let icmp_encapsulated_metrics = icmp_te_metrics.Icmp_te_metrics.icmp_encapsulated_metrics in

                  let (icmp_te_tcp_src_port, icmp_te_tcp_dst_port) =
                    Batteries.Option.map_default
                      (fun icmp_tcp_metrics ->
                         (icmp_tcp_metrics.Icmp_tcp_metrics.src_port,
                          icmp_tcp_metrics.Icmp_tcp_metrics.dst_port)
                      )
                      (Int_distribution.C.new_empty_t (),
                       Int_distribution.C.new_empty_t ())                    
                      icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_tcp_metrics_option
                  in

                  let (icmp_te_udp_src_port, icmp_te_udp_dst_port) =
                    Batteries.Option.map_default
                      (fun icmp_udp_metrics ->
                         (icmp_udp_metrics.Udp_metrics.src_port,
                          icmp_udp_metrics.Udp_metrics.dst_port)
                      )
                      (Int_distribution.C.new_empty_t (),
                       Int_distribution.C.new_empty_t ())                    
                      icmp_encapsulated_metrics.Icmp_encapsulated_metrics.icmp_udp_metrics_option
                  in

                  (
                    (Int_distribution.C.fusion
                       icmp_te_tcp_src_port
                       icmp_te_udp_src_port
                    )
                    ,
                    (Int_distribution.C.fusion
                       icmp_te_tcp_dst_port
                       icmp_te_udp_dst_port
                    )
                  )
               )
               (Int_distribution.C.new_empty_t (),
                Int_distribution.C.new_empty_t ())
               icmp_metrics.Icmp_metrics.icmp_te_metrics_option
           in

           (
             (Int_distribution.C.fusion
                (Int_distribution.C.fusion
                   icmp_du_src_port
                   icmp_rm_src_port
                )
                icmp_te_src_port
             )
             ,
             (Int_distribution.C.fusion
                (Int_distribution.C.fusion
                   icmp_du_dst_port
                   icmp_rm_dst_port
                )
                icmp_te_dst_port
             )
           )
        )
        (Int_distribution.C.new_empty_t (),
         Int_distribution.C.new_empty_t ())
        detailed_metrics.Detailed_metrics.icmp_metrics_option
    in

    let src_port, dst_port =
      Detailed_metrics.get_src_dst_port_distribution
        detailed_metrics
    in



    let src_ipaddr_with_biggest_occurence = Ipaddr.V4 (Ipaddr.V4.of_string_exn "0.0.0.0") in

    let null_ipaddr =
      match src_ipaddr_with_biggest_occurence with
      | Ipaddr.V4 _ -> Ipaddr.V4 (Ipaddr.V4.of_int32 Int32.zero)
      | Ipaddr.V6 _ -> Ipaddr.V6 (Ipaddr.V6.of_int64 (Int64.zero, Int64.zero))
    in

    let (most_common_src_addr, _) =
      Batteries.Option.map_default
        (fun (value, occurence) ->
           (Admd.Ipaddr_sb.to_string value)
           , 
           occurence
        )
        (Ipaddr.to_string null_ipaddr, 0)
        (Ipaddr_distribution.C.get_biggest_occurrence_tuple
           detailed_metrics.Detailed_metrics.src_addr)
    in
    let (most_common_dst_addr, _) =
      Batteries.Option.map_default
        (fun (value, occurence) ->
           (Admd.Ipaddr_sb.to_string value)
           , 
           occurence
        )
        (Ipaddr.to_string null_ipaddr, 0)
        (Ipaddr_distribution.C.get_biggest_occurrence_tuple
           detailed_metrics.Detailed_metrics.dst_addr)
    in
    let _most_common_src_addr_metric = Value_variant.String most_common_src_addr in
    let _most_common_dst_addr_metric = Value_variant.String most_common_dst_addr in

    let (most_common_src_port, most_common_src_port_occurence) =
      Batteries.Option.map_default
        (fun (value, occurence) -> (value, occurence))
        (-1, 0)
        (Int_distribution.C.get_biggest_occurrence_tuple src_port)
    in
    let (most_common_dst_port, most_common_dst_port_occurence) =
      Batteries.Option.map_default
        (fun (value, occurence) -> (value, occurence))
        (-1, 0)
        (Int_distribution.C.get_biggest_occurrence_tuple dst_port)
    in
    let most_common_src_port_metric = Value_variant.Int most_common_src_port in
    let most_common_dst_port_metric = Value_variant.Int most_common_dst_port in

    let (most_common_icmp_src_addr, _) =
      Batteries.Option.map_default
        (fun (value, occurence) -> 
           (Admd.Ipaddr_sb.to_string value)
           , 
           occurence
        )
        ("none", 0)
        (Ipaddr_distribution.C.get_biggest_occurrence_tuple icmp_src_addr)
    in
    let (most_common_icmp_dst_addr, _) =
      Batteries.Option.map_default
        (fun (value, occurence) ->
           (Admd.Ipaddr_sb.to_string value)
           , 
           occurence
        )
        ("none", 0)
        (Ipaddr_distribution.C.get_biggest_occurrence_tuple icmp_dst_addr)
    in
    let _most_common_icmp_src_addr_metric = Value_variant.String most_common_icmp_src_addr in
    let _most_common_icmp_dst_addr_metric = Value_variant.String most_common_icmp_dst_addr in

    let (most_common_icmp_src_port, most_common_icmp_src_port_occurence) =
      Batteries.Option.map_default
        (fun (value, occurence) -> (value, occurence))
        (-1, 0)
        (Int_distribution.C.get_biggest_occurrence_tuple icmp_src_port)
    in
    let (most_common_icmp_dst_port, most_common_icmp_dst_port_occurence) =
      Batteries.Option.map_default
        (fun (value, occurence) -> (value, occurence))
        (-1, 0)
        (Int_distribution.C.get_biggest_occurrence_tuple icmp_dst_port)
    in
    let most_common_icmp_src_port_metric = Value_variant.Int most_common_icmp_src_port in
    let most_common_icmp_dst_port_metric = Value_variant.Int most_common_icmp_dst_port in

    let list =
      [
        _most_common_src_addr_metric;
        _most_common_dst_addr_metric;
        most_common_src_port_metric;
        most_common_dst_port_metric;
        _most_common_icmp_src_addr_metric;
        _most_common_icmp_dst_addr_metric;
        most_common_icmp_src_port_metric;
        most_common_icmp_dst_port_metric;
      ]
    in
    let indice_value_tuple_list =
      List.mapi
        (fun indice value -> (indice , value))
        list
    in
    let int_metric_value_int_map =
      match Core.Std.Int.Map.of_alist indice_value_tuple_list with
      | `Duplicate_key indice ->
        print_endline (sprintf "Network_attributes: of_detailed_metrics: attribute already present with indice %d" indice);
        assert(false)
      | `Ok map -> map
    in

    (* debug "of_detailed_metrics: end"; *)

    let t =
      new_t
        (* attribute_int_map *)
        int_metric_value_int_map
    in

    (* verify  *)
    (*   ( *)
    (*     (Detailed_metrics.to_string To_string_mode.Simple detailed_metrics) *)
    (*     ^ *)
    (*     (to_string To_string_mode.Simple t) *)
    (*   ) *)
    (*   t; *)

    t
  )

(* TODO: UPDATE *)
(* let to_indice_name_tuple_array_classic _ = *)
(*   [| *)
(*     (0 , "nb_destinations"); *)
(*     (1 , "nb_sources"); *)
(*     (2 , "nb_packets_over_nb_diff_values_dst_port"); *)
(*     (3 , "nb_diff_src_addr_over_nb_diff_dst_addr"); *)
(*     (4 , "nb_icmp_packets_over_nb_packets"); *)
(*     (5 , "nb_echorequestreply_packets_over_nb_packets"); *)
(*     (6 , "nb_syn_packets_over_nb_packets"); *)
(*     (7 , "nb_rst_packets_over_nb_packets"); *)
(*     (8 , "biggest_dst_port_over_every_other_port"); *)
(*     (9 , "avg_nb_dst_port") *)
(*   |] *)

(* let get_value_of_attribute_id t attribute_id = *)
(*   (\* Int_map.find *\) *)
(*   (\*   attribute_id *\) *)
(*   (\*   t.attribute_int_map *\) *)
(*   Core.Std.Int.Map.find *)
(*     t.attribute_int_map *)
(*     attribute_id *)

(* let to_feature_array_classic *)
(*     t *)
(*     = *)
(*   (\* TO UPDATE *\) *)
(*   let attribute_indice_list = *)
(*     [| *)
(*       0; *)
(*       1; *)
(*       2; *)
(*       3; *)
(*       4; *)
(*       5; *)
(*       6; *)
(*       7; *)
(*       8; *)
(*       9; *)
(*     |] *)
(*   in *)
  
(*   let attribute_array = *)
(*     Array.map *)
(*       (fun attribute_indice -> Core.Std.Int.Map.find t.attribute_int_map attribute_indice) *)
(*       attribute_indice_list *)
(*   in *)
(*   attribute_array *)

(* let to_feature_array *)
(*     t *)
(*     = *)
(*   let bindings = Core.Std.Int.Map.to_alist t.attribute_int_map in *)
(*   let attribute_list = List.map snd bindings in *)
  
(*   Array.of_list attribute_list *)

let to_metric_array
    t
    =
  let bindings = Core.Std.Int.Map.to_alist t.map in
  let attribute_list = List.map snd bindings in
  
  Array.of_list attribute_list

(* let merge *)
(*     nb_src_ip_address_container *)
(*     nb_dst_ip_address_container *)
(*     nb_src_port *)
(*     nb_dst_port *)

(*     t_1 *)
(*     t_2 *)
(*   = *)
(*   ( *)
(*     let attribute_int_map = *)
(*       Core.Std.Int.Map.merge *)
(*         ~f: (fun ~key: key value -> *)
(*             match value with *)
(*             | `Both (value_1, value_2) -> *)
(*               Some *)
(*                 ( *)
(*                   match key with *)
(*                   (\* src_addr_entropy *\) *)
(*                   | 2 -> *)
(*                     0. *)
(*                   (\* dst_addr_entropy *\) *)
(*                   | 3 -> *)
(*                     0. *)
(*                   (\* start_time *\) *)
(*                   | 10 -> *)
(*                     min value_1 value_2 *)
(*                   (\* end_time *\) *)
(*                   | 11 -> *)
(*                     max value_1 value_2 *)
(*                   (\* (\\* nb_src_port *\\) *\) *)
(*                   (\* | 80 -> *\) *)
(*                   (\*   0. *\) *)
(*                   (\* (\\* nb_dst_port *\\) *\) *)
(*                   (\* | 81 -> *\) *)
(*                   (\*   0. *\) *)
(*                   (\* prop_src_port_system *\) *)
(*                   | 82 -> *)
(*                     0. *)
(*                   (\* prop_dst_port_system *\) *)
(*                   | 83 -> *)
(*                     0. *)
(*                   (\* biggest_src_port_over_every_other_port *\) *)
(*                   | 84 -> *)
(*                     0. *)
(*                   (\* biggest_dst_port_over_every_other_port *\) *)
(*                   | 85 -> *)
(*                     0. *)
(*                   | _ -> *)
(*                     value_1 +. value_2 *)
(*                 ) *)
(*             | `Left _ -> *)
(*               assert(false) *)
(*             | `Right _ -> *)
(*               assert(false) *)
(*           ) *)
(*         t_1.attribute_int_map *)
(*         t_2.attribute_int_map *)
(*     in *)

(*     let attribute_int_map = *)
(*       Core.Std.Int.Map.remove *)
(*         attribute_int_map *)
(*         0 *)
(*     in *)
(*     let attribute_int_map = *)
(*       Core.Std.Int.Map.add *)
(*         attribute_int_map *)
(*         0 *)
(*         nb_src_ip_address_container *)
(*     in *)
(*     let attribute_int_map = *)
(*       Core.Std.Int.Map.remove *)
(*         attribute_int_map *)
(*         1 *)
(*     in *)
(*     let attribute_int_map = *)
(*       Core.Std.Int.Map.add *)
(*         attribute_int_map *)
(*         1 *)
(*         nb_dst_ip_address_container *)
(*     in *)


(*     let attribute_int_map = *)
(*       Core.Std.Int.Map.remove *)
(*         attribute_int_map *)
(*         80 *)
(*     in *)
(*     let attribute_int_map = *)
(*       Core.Std.Int.Map.add *)
(*         attribute_int_map *)
(*         80 *)
(*         nb_src_port *)
(*     in *)
(*     let attribute_int_map = *)
(*       Core.Std.Int.Map.remove *)
(*         attribute_int_map *)
(*         81 *)
(*     in *)
(*     let attribute_int_map = *)
(*       Core.Std.Int.Map.add *)
(*         attribute_int_map *)
(*         81 *)
(*         nb_dst_port *)
(*     in *)
    
(*     let metric_int_map = *)
(*       t_1.metric_int_map *)
(*     in *)

(*     new_t *)
(*       attribute_int_map *)

(*       metric_int_map *)
(*   ) *)
