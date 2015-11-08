
open Printf

type t =
| ICMP of Icmp_metrics.t
| TCP of Five_tuple_flow_tcp_metrics.t
| UDP of Five_tuple_flow_udp_metrics.t
| IPv6 of Five_tuple_flow_ipv6_metrics.t
| GRE of Five_tuple_flow_gre_metrics.t
| ICMPv6 of Icmpv6_metrics.t

let to_string t =
  match t with
  | ICMP icmp_metrics -> 
    Icmp_metrics.to_string
      icmp_metrics
  | TCP five_tuple_flow_tcp_metrics -> 
    Five_tuple_flow_tcp_metrics.to_string
      five_tuple_flow_tcp_metrics
  | UDP five_tuple_flow_udp_metrics -> 
    Five_tuple_flow_udp_metrics.to_string
      five_tuple_flow_udp_metrics
  | IPv6 five_tuple_flow_ipv6_metrics -> 
    Five_tuple_flow_ipv6_metrics.to_string
      five_tuple_flow_ipv6_metrics
  | GRE five_tuple_flow_gre_metrics ->
    Five_tuple_flow_gre_metrics.to_string
      five_tuple_flow_gre_metrics
  | ICMPv6 icmpv6_metrics -> 
    Icmpv6_metrics.to_string
      icmpv6_metrics

let verify nb_packets error_string t =
  match t with
  | ICMP icmp_metrics -> 
    Icmp_metrics.verify
      nb_packets
      error_string
      icmp_metrics
  | TCP five_tuple_flow_tcp_metrics ->
    Five_tuple_flow_tcp_metrics.verify
      nb_packets
      error_string
      five_tuple_flow_tcp_metrics
  | UDP five_tuple_flow_udp_metrics -> 
    Five_tuple_flow_udp_metrics.verify
      nb_packets
      error_string
      five_tuple_flow_udp_metrics
  | IPv6 five_tuple_flow_ipv6_metrics -> 
    Five_tuple_flow_ipv6_metrics.verify
      nb_packets
      error_string
      five_tuple_flow_ipv6_metrics
  | GRE five_tuple_flow_gre_metrics ->
    Five_tuple_flow_gre_metrics.verify
      nb_packets
      error_string
      five_tuple_flow_gre_metrics
  | ICMPv6 icmpv6_metrics -> 
    Icmpv6_metrics.verify
      nb_packets
      error_string
      icmpv6_metrics
      
let to_int t = 
  match t with
  | ICMP _ -> 0
  | TCP _ -> 1
  | UDP _ -> 2
  | IPv6 _ -> 3
  | GRE _ -> 4
  | ICMPv6 _ -> 5

let copy t =
  match t with
  | ICMP icmp_metrics -> 
    ICMP
      (Icmp_metrics.copy
   icmp_metrics)
  | TCP five_tuple_flow_tcp_metrics -> 
    TCP 
      (Five_tuple_flow_tcp_metrics.copy
   five_tuple_flow_tcp_metrics)
  | UDP five_tuple_flow_udp_metrics -> 
    UDP
      (Five_tuple_flow_udp_metrics.copy
   five_tuple_flow_udp_metrics)
  | IPv6 five_tuple_flow_ipv6_metrics -> 
    IPv6
      (Five_tuple_flow_ipv6_metrics.copy
   five_tuple_flow_ipv6_metrics)
  | GRE five_tuple_flow_gre_metrics -> 
    GRE
      (Five_tuple_flow_gre_metrics.copy
   five_tuple_flow_gre_metrics)
  | ICMPv6 icmpv6_metrics -> 
    ICMPv6
      (Icmpv6_metrics.copy
   icmpv6_metrics)

(* TODO: change names *)
let fusion t t_to_append =
  match t with
  | ICMP icmp_metrics -> 
    (match t_to_append with
     | ICMP icmp_metrics_2 ->
       ICMP
         (Icmp_metrics.fusion
            icmp_metrics
            icmp_metrics_2)
     | TCP _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmp_metrics and five_tuple_flow_tcp_metrics"
     | UDP _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmp_metrics and five_tuple_flow_udp_metrics"
     | IPv6 _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmp_metrics and five_tuple_flow_ipv6_metrics"
     | GRE _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmp_metrics and five_tuple_flow_gre_metrics"
     | ICMPv6 _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmp_metrics and five_tuple_flow_icmpv6_metrics"
    )
  | TCP five_tuple_flow_tcp_metrics_1 -> 
    (match t_to_append with
     | ICMP icmp_metrics_2 ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_tcp_metrics and icmp_metrics"
     | TCP five_tuple_flow_tcp_metrics_2 ->
       TCP
         (Five_tuple_flow_tcp_metrics.fusion
            five_tuple_flow_tcp_metrics_1
            five_tuple_flow_tcp_metrics_2)
     | UDP five_tuple_flow_udp_metrics_2 ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_tcp_metrics and five_tuple_flow_udp_metrics"
     | IPv6 five_tuple_flow_ipv6_metrics_2 ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_tcp_metrics and five_tuple_flow_ipv6_metrics"
     | GRE _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_tcp_metrics and five_tuple_flow_gre_metrics"
     | ICMPv6 _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_tcp_metrics and five_tuple_flow_icmpv6_metrics"
    )
  | UDP five_tuple_flow_udp_metrics_1 -> 
    (match t_to_append with
     | ICMP icmp_metrics_2 ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_udp_metrics and icmp_metrics"
     | TCP five_tuple_flow_tcp_metrics_2 ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_udp_metrics and five_tuple_flow_tcp_metrics"
     | UDP five_tuple_flow_udp_metrics_2 -> 
       UDP 
         (Five_tuple_flow_udp_metrics.fusion
            five_tuple_flow_udp_metrics_1
            five_tuple_flow_udp_metrics_2)
     | IPv6 icmp_metrics_2 ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_udp_metrics and five_tuple_flow_ipv6_metrics"
     | GRE _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_udp_metrics and five_tuple_flow_gre_metrics"
     | ICMPv6 _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_udp_metrics and five_tuple_flow_icmpv6_metrics"
    )
  | IPv6 five_tuple_flow_ipv6_metrics_1 -> 
    (match t_to_append with
     | ICMP icmp_metrics_2 ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_ipv6_metrics and icmp_metrics"
     | TCP five_tuple_flow_tcp_metrics_2 ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_ipv6_metrics and five_tuple_flow_tcp_metrics"
     | UDP five_tuple_flow_udp_metrics_2 -> 
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_ipv6_metrics and five_tuple_flow_tcp_metrics"
     | IPv6 five_tuple_flow_ipv6_metrics_2 ->
       IPv6 
         (Five_tuple_flow_ipv6_metrics.fusion
            five_tuple_flow_ipv6_metrics_1
            five_tuple_flow_ipv6_metrics_2)
     | GRE _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_ipv6_metrics and five_tuple_flow_gre_metrics"
     | ICMPv6 _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_ipv6_metrics and five_tuple_flow_icmpv6_metrics"
    )
  | GRE five_tuple_flow_gre_metrics_1 -> 
    (match t_to_append with
     | ICMP icmp_metrics_2 ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_gre_metrics and icmp_metrics"
     | TCP five_tuple_flow_tcp_metrics_2 ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_gre_metrics and five_tuple_flow_tcp_metrics"
     | UDP five_tuple_flow_udp_metrics_2 -> 
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_gre_metrics and five_tuple_flow_tcp_metrics"
     | IPv6 _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_gre_metrics and five_tuple_flow_ipv6_metrics"
     | GRE five_tuple_flow_gre_metrics_2 ->
       GRE
         (Five_tuple_flow_gre_metrics.fusion
            five_tuple_flow_gre_metrics_1
            five_tuple_flow_gre_metrics_2)
     | ICMPv6 _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_gre_metrics and five_tuple_flow_icmpv6_metrics"
    )
  | ICMPv6 icmpv6_metrics -> 
    (match t_to_append with
     | ICMP _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmpv6_metrics and five_tuple_flow_icmp_metrics"
     | TCP _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmpv6_metrics and five_tuple_flow_tcp_metrics"
     | UDP _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmpv6_metrics and five_tuple_flow_udp_metrics"
     | IPv6 _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmpv6_metrics and five_tuple_flow_ipv6_metrics"
     | GRE _ ->
       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmpv6_metrics and five_tuple_flow_gre_metrics"
     | ICMPv6 icmpv6_metrics_2 ->
       ICMPv6
         (Icmpv6_metrics.fusion
            icmpv6_metrics
            icmpv6_metrics_2)
    )
    
(* let fusion t_1 t_2 = *)
(*   match t_1 with *)
(*   | ICMP icmp_metrics_1 ->  *)
(*     (match t_2 with *)
(*     | UDP five_tuple_flow_udp_metrics_2 ->  *)
(*       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmp_metrics and five_tuple_flow_udp_metrics" *)
(*     | TCP five_tuple_flow_tcp_metrics_2 -> *)
(*       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_icmp_metrics and five_tuple_flow_tcp_metrics" *)
(*     | ICMP icmp_metrics_2 -> *)
(*       ICMP *)
(*         (Icmp_metrics.fusion *)
(*            icmp_metrics_1 *)
(*            icmp_metrics_2) *)
(*     ) *)
(*   | TCP five_tuple_flow_tcp_metrics_1 ->  *)
(*     (match t_2 with *)
(*     | UDP five_tuple_flow_udp_metrics_2 ->  *)
(*       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_tcp_metrics and five_tuple_flow_udp_metrics" *)
(*     | TCP five_tuple_flow_tcp_metrics_2 -> *)
(*       TCP *)
(*         (Five_tuple_flow_tcp_metrics.fusion *)
(*            five_tuple_flow_tcp_metrics_1 *)
(*            five_tuple_flow_tcp_metrics_2) *)
(*     | ICMP icmp_metrics_2 -> *)
(*       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_tcp_metrics and icmp_metrics" *)
(*     ) *)
(*   | UDP five_tuple_flow_udp_metrics_1 ->  *)
(*     (match t_2 with *)
(*     | UDP five_tuple_flow_udp_metrics_2 ->  *)
(*       UDP  *)
(*         (Five_tuple_flow_udp_metrics.fusion *)
(*            five_tuple_flow_udp_metrics_1 *)
(*            five_tuple_flow_udp_metrics_2) *)
(*     | TCP five_tuple_flow_tcp_metrics_2 -> *)
(*       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_udp_metrics and five_tuple_flow_tcp_metrics" *)
(*     | ICMP icmp_metrics_2 -> *)
(*       failwith "Transport_layer_metrics: fusion cannot fusion five_tuple_flow_udp_metrics and icmp_metrics" *)
(*     ) *)

let of_packet_data_for_metrics
    packet_data_for_metrics
  =
  (
    match packet_data_for_metrics.Packet_data_for_metrics.pdu_t with
    | Packet_data_for_metrics.IPV4 ipv4_data_for_metrics ->
      (
        let fragmentation, first_fragment =
          Ipv4_data_for_metrics.get_fragmentation_boolean
            ipv4_data_for_metrics
        in

        match ipv4_data_for_metrics.Ipv4_data_for_metrics.pdu_t with
        | Ipv4_data_for_metrics.ICMP icmp_data_for_metrics ->
          (
            let icmp_metrics = Icmp_metrics.new_single_t () in
            Icmp_metrics.update_icmp_data_for_metrics icmp_metrics icmp_data_for_metrics;
            ICMP icmp_metrics            
          )
        | Ipv4_data_for_metrics.TCP tcp_data_for_metrics ->
          (
            let five_tuple_flow_tcp_metrics =
              Five_tuple_flow_tcp_metrics.of_tcp_data_for_metrics
                tcp_data_for_metrics
            in
            TCP five_tuple_flow_tcp_metrics
          )
        | Ipv4_data_for_metrics.UDP udp_data_for_metrics ->
          (
            let five_tuple_flow_udp_metrics = Five_tuple_flow_udp_metrics.new_empty_t () in
            Five_tuple_flow_udp_metrics.update
              five_tuple_flow_udp_metrics

              fragmentation
              first_fragment

              udp_data_for_metrics;
            UDP five_tuple_flow_udp_metrics
          )
        | Ipv4_data_for_metrics.IPv6 ipv6_data_for_metrics ->
          (
            let five_tuple_flow_ipv6_metrics = Five_tuple_flow_ipv6_metrics.new_empty_t () in
            Five_tuple_flow_ipv6_metrics.update five_tuple_flow_ipv6_metrics ipv6_data_for_metrics;
            IPv6 five_tuple_flow_ipv6_metrics
          )
        | Ipv4_data_for_metrics.GRE ->
          (
            let five_tuple_flow_gre_metrics = Five_tuple_flow_gre_metrics.new_empty_t () in
            Five_tuple_flow_gre_metrics.update five_tuple_flow_gre_metrics;
            GRE five_tuple_flow_gre_metrics
          )
        | Ipv4_data_for_metrics.Other protocol_number -> assert(false)
      )
    | Packet_data_for_metrics.IPV6 ipv6_data_for_metrics ->
      (
        match ipv6_data_for_metrics.Ipv6_data_for_metrics.pdu_t with
        | Ipv6_data_for_metrics.ICMP icmp_data_for_metrics ->
          (
            let icmp_metrics = Icmp_metrics.new_single_t () in
            Icmp_metrics.update_icmp_data_for_metrics icmp_metrics icmp_data_for_metrics;
            ICMP icmp_metrics            
          )
        | Ipv6_data_for_metrics.TCP tcp_data_for_metrics ->
          (
            let five_tuple_flow_tcp_metrics = Five_tuple_flow_tcp_metrics.new_empty_t () in
            Five_tuple_flow_tcp_metrics.update five_tuple_flow_tcp_metrics tcp_data_for_metrics;
            TCP five_tuple_flow_tcp_metrics
          )
        | Ipv6_data_for_metrics.UDP udp_data_for_metrics ->
          (
            let five_tuple_flow_udp_metrics = Five_tuple_flow_udp_metrics.new_empty_t () in
            Five_tuple_flow_udp_metrics.update
              five_tuple_flow_udp_metrics

              false
              false

              udp_data_for_metrics;
            UDP five_tuple_flow_udp_metrics
          )
        | Ipv6_data_for_metrics.GRE ->
          (
            let five_tuple_flow_gre_metrics = Five_tuple_flow_gre_metrics.new_empty_t () in
            Five_tuple_flow_gre_metrics.update five_tuple_flow_gre_metrics;
            GRE five_tuple_flow_gre_metrics
          )
        | Ipv6_data_for_metrics.ICMP6 icmpv6_data_for_metrics ->

          (
            let icmpv6_metrics = Icmpv6_metrics.new_single_t () in
            Icmpv6_metrics.update_icmpv6_data_for_metrics icmpv6_metrics icmpv6_data_for_metrics;
            ICMPv6 icmpv6_metrics            
          )
        | Ipv6_data_for_metrics.Other protocol_number -> assert(false)
      )
    (* | Packet_data_for_metrics.IP _ -> assert(false) *)
    | Packet_data_for_metrics.Other -> assert(false)
  )

(* TODO: add _ when unused *)
let update
    t
    packet_data_for_metrics
  =
  match packet_data_for_metrics.Packet_data_for_metrics.pdu_t with
  | Packet_data_for_metrics.IPV4 ipv4_data_for_metrics ->
    (
      let fragmentation, first_fragment =
        Ipv4_data_for_metrics.get_fragmentation_boolean
          ipv4_data_for_metrics
      in

      match ipv4_data_for_metrics.Ipv4_data_for_metrics.pdu_t with
      | Ipv4_data_for_metrics.ICMP icmp_data_for_metrics ->
        (
          match t with
          | ICMP icmp_metrics ->
            Icmp_metrics.update_icmp_data_for_metrics icmp_metrics icmp_data_for_metrics;
          | TCP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding ICMP packet to TCP flow"
          | UDP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding ICMP packet to UDP flow"
          | IPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding ICMP packet to IPv6 flow"
          | GRE _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding ICMP packet to GRE flow"
          | ICMPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding ICMP packet to ICMPv6 flow"
        )
      | Ipv4_data_for_metrics.TCP tcp_for_metrics ->
        (
          match t with
          | ICMP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding TCP packet to ICMP flow"
          | TCP five_tuple_flow_tcp_metrics ->
            Five_tuple_flow_tcp_metrics.update five_tuple_flow_tcp_metrics tcp_for_metrics;
            (* TCP five_tuple_flow_tcp_metrics *)
          | UDP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding TCP packet to UDP flow"
          | IPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding TCP packet to IPv6 flow"
          | GRE _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding TCP packet to GRE flow"
          | ICMPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding TCP packet to ICMPv6 flow"
        )
      | Ipv4_data_for_metrics.UDP udp_for_metrics ->
        (
          match t with
          | ICMP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding UDP packet to ICMP flow"
          | TCP five_tuple_flow_tcp_metrics ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding UDP packet to TCP flow"
          | UDP five_tuple_flow_udp_metrics ->
            Five_tuple_flow_udp_metrics.update
              five_tuple_flow_udp_metrics

              fragmentation
              first_fragment

              udp_for_metrics;
            (* UDP five_tuple_flow_udp_metrics *)
          | IPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding UDP packet to IPv6 flow"
          | GRE _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding UDP packet to GRE flow"
          | ICMPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding UDP packet to ICMPv6 flow"
        )
      | Ipv4_data_for_metrics.IPv6 ipv6_data_for_metrics ->
        (
          match t with
          | ICMP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding IPv6 packet to ICMP flow"
          | TCP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding IPv6 packet to TCP flow"
          | UDP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding IPv6 packet to UDP flow"
          | IPv6 five_tuple_flow_ipv6_metrics ->
            Five_tuple_flow_ipv6_metrics.update five_tuple_flow_ipv6_metrics ipv6_data_for_metrics;
          | GRE _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding IPv6 packet to GRE flow"
          | ICMPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding IPv6 packet to ICMPv6 flow"
        )
      | Ipv4_data_for_metrics.GRE ->
        (
          match t with
          | ICMP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to ICMP flow"
          | TCP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to TCP flow"
          | UDP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to UDP flow"
          | IPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to IPv6 flow"
          | GRE five_tuple_flow_gre_metrics ->
            Five_tuple_flow_gre_metrics.update five_tuple_flow_gre_metrics;
          | ICMPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to ICMPv6 flow"
        )
      | Ipv4_data_for_metrics.Other protocol_number ->
        exit 11
    )
  | Packet_data_for_metrics.IPV6 ipv6_data_for_metrics ->
    (
      match ipv6_data_for_metrics.Ipv6_data_for_metrics.pdu_t with
      | Ipv6_data_for_metrics.ICMP icmp_data_for_metrics ->
        (
          match t with
          | ICMP icmp_metrics ->
            Icmp_metrics.update_icmp_data_for_metrics icmp_metrics icmp_data_for_metrics;
          | TCP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding ICMP packet to TCP flow"
          | UDP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding ICMP packet to UDP flow"
          | IPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding ICMP packet to IPv6 flow"
          | GRE _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding ICMP packet to GRE flow"
          | ICMPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding ICMP packet to ICMPv6 flow"
        )
      | Ipv6_data_for_metrics.TCP tcp_for_metrics ->
        (
          match t with
          | ICMP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding TCP packet to ICMP flow"
          | TCP five_tuple_flow_tcp_metrics ->
            Five_tuple_flow_tcp_metrics.update five_tuple_flow_tcp_metrics tcp_for_metrics;
          | UDP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding TCP packet to UDP flow"
          | IPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding TCP packet to IPv6 flow"
          | GRE _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding TCP packet to GRE flow"
          | ICMPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding TCP packet to ICMPv6 flow"
        )
      | Ipv6_data_for_metrics.UDP udp_for_metrics ->
        (
          match t with
          | ICMP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding UDP packet to ICMP flow"
          | TCP five_tuple_flow_tcp_metrics ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding UDP packet to TCP flow"
          | UDP five_tuple_flow_udp_metrics ->
            Five_tuple_flow_udp_metrics.update
              five_tuple_flow_udp_metrics

              false
              false
              
              udp_for_metrics;
          | IPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding UDP packet to IPv6 flow"
          | GRE _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding UDP packet to GRE flow"
          | ICMPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding UDP packet to ICMPv6 flow"
        )
      | Ipv6_data_for_metrics.GRE ->
        (
          match t with
          | ICMP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to ICMP flow"
          | TCP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to TCP flow"
          | UDP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to UDP flow"
          | IPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to IPv6 flow"
          | GRE five_tuple_flow_gre_metrics ->
            Five_tuple_flow_gre_metrics.update five_tuple_flow_gre_metrics;
          | ICMPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to ICMPv6 flow"
        )
      | Ipv6_data_for_metrics.ICMP6 icmp6_data_for_metrics ->
        (
          match t with
          | ICMP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to ICMP flow"
          | TCP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to TCP flow"
          | UDP _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to UDP flow"
          | IPv6 _ ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to IPv6 flow"
          | GRE five_tuple_flow_gre_metrics ->
            failwith "Five_tuple_flow_detailed_metrics: update_melange: adding GRE packet to ICMPv6 flow"
          | ICMPv6 icmpv6_metrics ->
            Icmpv6_metrics.update_icmpv6_data_for_metrics icmpv6_metrics icmp6_data_for_metrics;
        )
      | Ipv6_data_for_metrics.Other protocol_number ->
        exit 11
    )
  | Packet_data_for_metrics.Other ->
    exit 12
