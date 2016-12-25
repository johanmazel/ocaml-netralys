
open Printf

module A = BatArray
module S = BatString
module HT = BatHashtbl
module L = BatList

open Sexplib.Std

open Key_occurrence_distribution_instantiations
open Network_traffic_metric_instantiations

let use_verification = ref false
    
type t =
  {
    mutable timestamp_sec_start : int;
    mutable timestamp_usec_start : int;
    mutable timestamp_sec_end : int;
    mutable timestamp_usec_end : int;

    mutable nb_packets : int;
    mutable nb_bytes : int;
    (* packet_size_distribution : Int_distribution.C.t; *)

    packet_fingerprint_distribution : Packet_fingerprint_distribution.C.t;

    src_addr : Ipaddr_distribution.C.t;
    dst_addr : Ipaddr_distribution.C.t;

    transport_protocol_for_metrics_distribution : Transport_protocol_for_metrics_distribution.C.t;

    mutable icmp_metrics_option : Icmp_metrics.t option;
    mutable tcp_metrics_option : Tcp_metrics.t option;
    mutable udp_metrics_option : Udp_metrics.t option;
    mutable ipv6_metrics_option : Ipv6_metrics.t option;
    mutable gre_metrics_option : Gre_metrics.t option;
    mutable icmpv6_metrics_option : Icmpv6_metrics.t option;
    mutable other_protocol_metrics_option : Other_protocol_metrics.t option;
  }
[@@deriving compare, sexp]

let new_t
    timestamp_sec_start
    timestamp_usec_start
    timestamp_sec_end
    timestamp_usec_end

    nb_packets
    nb_bytes
    (* packet_size_distribution *)

    packet_fingerprint_distribution

    src_addr
    dst_addr

    transport_protocol_for_metrics_distribution

    icmp_metrics_option
    tcp_metrics_option
    udp_metrics_option
    ipv6_metrics_option
    gre_metrics_option
    icmpv6_metrics_option
    other_protocol_metrics_option
  =
  {
    timestamp_sec_start;
    timestamp_usec_start;
    timestamp_sec_end;
    timestamp_usec_end;

    nb_packets;
    nb_bytes;
    (* packet_size_distribution; *)

    packet_fingerprint_distribution;

    src_addr;
    dst_addr;

    transport_protocol_for_metrics_distribution;

    icmp_metrics_option;
    tcp_metrics_option;
    udp_metrics_option;
    ipv6_metrics_option;
    gre_metrics_option;
    icmpv6_metrics_option;
    other_protocol_metrics_option;
  }

let new_empty_t () =
  let r : t =
    new_t
      0
      0
      0
      0

      0
      0
      (* (Int_distribution.C.new_single_t ()) *)

      (Packet_fingerprint_distribution.C.new_single_t ())

      (Ipaddr_distribution.C.new_single_t ())
      (Ipaddr_distribution.C.new_single_t ())

      (Transport_protocol_for_metrics_distribution.C.new_single_t ())

      None
      None
      None
      None
      None
      None
      None
  in
  r
    
let to_string
    t
  =
  let span_start = 
    Core.Span.create
      ~sec: t.timestamp_sec_start
      ~us: t.timestamp_usec_start
      ()
  in
  let time_start = Core.Time.add Core.Time.epoch span_start in
  let span_end = 
    Core.Span.create
      ~sec: t.timestamp_sec_end
      ~us: t.timestamp_usec_end
      ()
  in
  let time_end = Core.Time.add Core.Time.epoch span_end in

  sprintf
    "%d.%d (%s)\n%d.%d (%s)\nnb_packets: %d\nnb_bytes: %d\npacket_fingerprint: %s\nsrc_addr: %s\ndst_addr: %s\ntransport_protocol: %s\n%s\n%s\n%s\n%s\n%s\n%s\n%s"
    t.timestamp_sec_start
    t.timestamp_usec_start
    (Core.Time.to_string_trimmed ~zone: Core.Time.Zone.local time_start)
    t.timestamp_sec_end
    t.timestamp_usec_end
    (Core.Time.to_string_trimmed ~zone: Core.Time.Zone.local time_end)
    t.nb_packets
    t.nb_bytes
    (* (Int_distribution.C.to_string to_string_mode t.packet_size_distribution) *)
    (Packet_fingerprint_distribution.C.to_string t.packet_fingerprint_distribution)
    (Ipaddr_distribution.C.to_string t.src_addr)
    (Ipaddr_distribution.C.to_string t.dst_addr)
    (Transport_protocol_for_metrics_distribution.C.to_string t.transport_protocol_for_metrics_distribution)
    (match t.icmp_metrics_option with
     | None -> ""
     | Some icmp_metrics -> "ICMP:\n" ^ Icmp_metrics.to_string icmp_metrics ^ "\n"
    )
    (match t.tcp_metrics_option with
     | None -> ""
     | Some tcp_metrics -> "TCP:\n" ^ Tcp_metrics.to_string tcp_metrics ^ "\n"
    )
    (match t.udp_metrics_option with
     | None -> ""
     | Some udp_metrics -> "UDP:\n" ^ Udp_metrics.to_string udp_metrics ^ "\n"
    )
    (match t.ipv6_metrics_option with
     | None -> ""
     | Some ipv6_metrics -> "IPv6:\n" ^ Ipv6_metrics.to_string ipv6_metrics ^ "\n"
    )
    (match t.gre_metrics_option with
     | None -> ""
     | Some gre_metrics -> "GRE:\n" ^ Gre_metrics.to_string gre_metrics ^ "\n"
    )
    (match t.icmpv6_metrics_option with
     | None -> ""
     | Some icmpv6_metrics -> "ICMPv6:\n" ^ Icmpv6_metrics.to_string icmpv6_metrics ^ "\n"
    )
    (match t.other_protocol_metrics_option with
     | None -> ""
     | Some other_protocol_metrics -> "Other:\n" ^ Other_protocol_metrics.to_string other_protocol_metrics ^ "\n"
    )
 
let verify error_string t =
  (
    (* If the anomaly is not empty, we check that no timestamp is
       equal to epoch *)
    if
      (
        (t.nb_packets > 0)
        &&
        ((t.timestamp_sec_start = 0 && t.timestamp_usec_start = 0) || (t.timestamp_sec_end = 0 && t.timestamp_usec_end = 0))
      ) 
    then
      (
        print_endline
          (sprintf
             "Detailed_metrics: verify: timestamp equal to epoch :\n%s"
             (to_string
                t
             )
          );
        assert(false)
      );

    let number_packet_fingerprint = Packet_fingerprint_distribution.C.get_total_nb_occurrence t.packet_fingerprint_distribution in
    if number_packet_fingerprint <> t.nb_packets then
      (
        print_endline
          (sprintf
             "Detailed_metrics: verify: inconsistency between number of packet fingerprint occurence (%d) and total number of packets (%d):\n%s\n\n%s\n\n"
             number_packet_fingerprint
             t.nb_packets
             (to_string
                t
             )
             error_string
          );

        assert(false)
      );

    let number_src_addr = Ipaddr_distribution.C.get_total_nb_occurrence t.src_addr in
    if number_src_addr <> t.nb_packets then
      (
        print_endline
          (sprintf
             "Detailed_metrics: verify: inconsistency between number of src addr occurence (%d) and total number of packets (%d):\n%s\n\n%s\n\n"
             number_src_addr
             t.nb_packets
             (to_string
                t
             )
             error_string
          );

        assert(false)
      );
    let number_dst_addr = Ipaddr_distribution.C.get_total_nb_occurrence t.dst_addr in
    if number_dst_addr <> t.nb_packets then
      (
        print_endline
          (sprintf
             "Detailed_metrics: verify: inconsistency between number of dst addr occurence (%d) and total number of packets (%d):\n%s\n\n%s\n\n"
             number_dst_addr
             t.nb_packets
             (to_string
                t
             )
             error_string
          );
        assert(false)
      );

    let number_transport_protocol = Transport_protocol_for_metrics_distribution.C.get_total_nb_occurrence t.transport_protocol_for_metrics_distribution in
    if number_transport_protocol <> t.nb_packets then
      (
        print_endline
          (sprintf
             "Detailed_metrics: verify: inconsistency between number of transport protocol occurence (%d) and total number of packets (%d):\n%s\n\n"
             number_transport_protocol
             t.nb_packets
             (to_string
                t
             )
          );
        assert(false)
      );

    let icmp_packets_number =
      Transport_protocol_for_metrics_distribution.C.get_key_occurrence
        Transport_protocol_for_metrics.ICMP
        t.transport_protocol_for_metrics_distribution
    in
    let icmpv6_packets_number =
      Transport_protocol_for_metrics_distribution.C.get_key_occurrence
        Transport_protocol_for_metrics.ICMPv6
        t.transport_protocol_for_metrics_distribution
    in

    Batteries.Option.may (Icmp_metrics.verify icmp_packets_number error_string) t.icmp_metrics_option;
    Batteries.Option.may Tcp_metrics.verify t.tcp_metrics_option;
    Batteries.Option.may Udp_metrics.verify t.udp_metrics_option;
    Batteries.Option.may (Icmpv6_metrics.verify icmpv6_packets_number error_string) t.icmpv6_metrics_option;

    let nb_transport_layer_packet =
      (Batteries.Option.map_default
         (fun icmp_metrics -> Icmp_metrics.get_nb_packets icmp_metrics)
         0
         t.icmp_metrics_option
      )
      +
      (Batteries.Option.map_default
         (fun tcp_metrics -> tcp_metrics.Tcp_metrics.nb_tcp_packets)
         0
         t.tcp_metrics_option
      )
      +
      (Batteries.Option.map_default
         (fun udp_metrics -> udp_metrics.Udp_metrics.nb_udp_packets)
         0
         t.udp_metrics_option
      )
      +
      (Batteries.Option.map_default
         (fun ipv6_metrics -> ipv6_metrics.Ipv6_metrics.nb_packets)
         0
         t.ipv6_metrics_option
      )
      +
      (Batteries.Option.map_default
         (fun gre_metrics -> gre_metrics.Gre_metrics.nb_packets)
         0
         t.gre_metrics_option
      )
      +
      (Batteries.Option.map_default
         (fun icmpv6_metrics -> Icmpv6_metrics.get_nb_packets icmpv6_metrics)
         0
         t.icmpv6_metrics_option
      )
      +
      (Batteries.Option.map_default
         (fun other_protocol_metrics -> other_protocol_metrics.Other_protocol_metrics.nb_packets)
         0
         t.other_protocol_metrics_option
      )
    in
    if nb_transport_layer_packet <> t.nb_packets then
      (
        print_endline
          (sprintf
             "Detailed_metrics: verify: inconsistency between total number of transport layer packets (%d) and total number of packets (%d):\n%s\n\n"
             nb_transport_layer_packet
             t.nb_packets
             (to_string
                t
             )
          );
        assert(false)
      );
  )

let get_src_address_list t =
  Ipaddr_distribution.C.keys
    t.src_addr

let get_dst_address_list t =
  Ipaddr_distribution.C.keys
    t.dst_addr

let copy t =
  let r : t =
  new_t
    t.timestamp_sec_start
    t.timestamp_usec_start
    t.timestamp_sec_end
    t.timestamp_usec_end

    t.nb_packets
    t.nb_bytes
    
    (Packet_fingerprint_distribution.C.copy t.packet_fingerprint_distribution)

    (Ipaddr_distribution.C.copy t.src_addr)
    (Ipaddr_distribution.C.copy t.dst_addr)

    (Transport_protocol_for_metrics_distribution.C.copy t.transport_protocol_for_metrics_distribution)

    (Batteries.Option.map Icmp_metrics.copy t.icmp_metrics_option)
    (Batteries.Option.map Tcp_metrics.copy t.tcp_metrics_option)
    (Batteries.Option.map Udp_metrics.copy t.udp_metrics_option)
    (Batteries.Option.map Ipv6_metrics.copy t.ipv6_metrics_option)
    (Batteries.Option.map Gre_metrics.copy t.gre_metrics_option)
    (Batteries.Option.map Icmpv6_metrics.copy t.icmpv6_metrics_option)
    (Batteries.Option.map Other_protocol_metrics.copy t.other_protocol_metrics_option)
  in
  r

let append
    t
    t_to_append
  =
  (
    let (timestamp_sec_start, timestamp_usec_start) =
      if t.timestamp_sec_start <> t_to_append.timestamp_sec_start then
        (
          if t.timestamp_sec_start < t_to_append.timestamp_sec_start then
            (t.timestamp_sec_start, t.timestamp_usec_start)
          else
            (t_to_append.timestamp_sec_start, t_to_append.timestamp_usec_start)
        )
      else
        (t.timestamp_sec_start 
         ,
         min 
           t.timestamp_usec_start 
           t_to_append.timestamp_usec_start 
        )
    in
    let (timestamp_sec_end, timestamp_usec_end) =
      if t.timestamp_sec_end <> t_to_append.timestamp_sec_end then
        (
          if t.timestamp_sec_end > t_to_append.timestamp_sec_end then
            (t.timestamp_sec_end, t.timestamp_usec_end)
          else
            (t_to_append.timestamp_sec_end, t_to_append.timestamp_usec_end)
        )
      else
        (t.timestamp_sec_end
         ,
         max
           t.timestamp_usec_end
           t_to_append.timestamp_usec_end
        )
    in

    t.timestamp_sec_start <- timestamp_sec_start;
    t.timestamp_usec_start <- timestamp_usec_start;

    t.timestamp_sec_end <- timestamp_sec_end;
    t.timestamp_usec_end <- timestamp_usec_end;

    t.nb_packets <- t.nb_packets + t_to_append.nb_packets;
    t.nb_bytes <- t.nb_bytes + t_to_append.nb_bytes;
    
    Packet_fingerprint_distribution.C.append
      t.packet_fingerprint_distribution
      t_to_append.packet_fingerprint_distribution;

    Ipaddr_distribution.C.append
      t.src_addr
      t_to_append.src_addr;
    Ipaddr_distribution.C.append
      t.dst_addr
      t_to_append.dst_addr;

    Transport_protocol_for_metrics_distribution.C.append
      t.transport_protocol_for_metrics_distribution
      t_to_append.transport_protocol_for_metrics_distribution;

    Option_utils.append
      Icmp_metrics.append
      (fun icmp_metrics_to_append ->
         t.icmp_metrics_option <- Some (Icmp_metrics.copy icmp_metrics_to_append)
      )
      t.icmp_metrics_option
      t_to_append.icmp_metrics_option;
    Option_utils.append
      Tcp_metrics.append
      (fun tcp_metrics_to_append ->
         t.tcp_metrics_option <- Some (Tcp_metrics.copy tcp_metrics_to_append)
      )
      t.tcp_metrics_option
      t_to_append.tcp_metrics_option;
    Option_utils.append
      Udp_metrics.append
      (fun udp_metrics_to_append ->
         t.udp_metrics_option <- Some (Udp_metrics.copy udp_metrics_to_append)
      )
      t.udp_metrics_option
      t_to_append.udp_metrics_option;
    Option_utils.append
      Ipv6_metrics.append
      (fun ipv6_metrics_to_append ->
         t.ipv6_metrics_option <- Some (Ipv6_metrics.copy ipv6_metrics_to_append)
      )
      t.ipv6_metrics_option
      t_to_append.ipv6_metrics_option;
    Option_utils.append
      Gre_metrics.append
      (fun gre_metrics_to_append ->
         t.gre_metrics_option <- Some (Gre_metrics.copy gre_metrics_to_append)
      )
      t.gre_metrics_option
      t_to_append.gre_metrics_option;
    Option_utils.append
      Icmpv6_metrics.append
      (fun icmpv6_metrics_to_append ->
         t.icmpv6_metrics_option <- Some (Icmpv6_metrics.copy icmpv6_metrics_to_append)
      )
      t.icmpv6_metrics_option
      t_to_append.icmpv6_metrics_option;
    Option_utils.append
      Other_protocol_metrics.append
      (fun other_protocol_metrics_to_append ->
         t.other_protocol_metrics_option <- Some (Other_protocol_metrics.copy other_protocol_metrics_to_append)
      )
      t.other_protocol_metrics_option
      t_to_append.other_protocol_metrics_option;


    if !use_verification then
      verify "" t;
  )

let fusion
    t1
    t2
  =
  (
    let (timestamp_sec_start, timestamp_usec_start) =
      if t1.timestamp_sec_start <> t2.timestamp_sec_start then
        (
          if t1.timestamp_sec_start < t2.timestamp_sec_start then
            (t1.timestamp_sec_start, t1.timestamp_usec_start)
          else
            (t2.timestamp_sec_start, t2.timestamp_usec_start)
        )
      else
        (t1.timestamp_sec_start 
         ,
         min 
           t1.timestamp_usec_start 
           t2.timestamp_usec_start 
        )
    in
    let (timestamp_sec_end, timestamp_usec_end) =
      if t1.timestamp_sec_end <> t2.timestamp_sec_end then
        (
          if t1.timestamp_sec_end > t2.timestamp_sec_end then
            (t1.timestamp_sec_end, t1.timestamp_usec_end)
          else
            (t2.timestamp_sec_end, t2.timestamp_usec_end)
        )
      else
        (t1.timestamp_sec_end
         ,
         max
           t1.timestamp_usec_end
           t2.timestamp_usec_end
        )
    in

    let nb_packets =
      t1.nb_packets
      +
      t2.nb_packets
    in
    let nb_bytes =
      t1.nb_bytes
      +
      t2.nb_bytes
    in

    let packet_fingerprint =
      Packet_fingerprint_distribution.C.fusion
        t1.packet_fingerprint_distribution
        t2.packet_fingerprint_distribution
    in

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

    let transport_protocol_for_metrics_distribution =
      Transport_protocol_for_metrics_distribution.C.fusion
        t1.transport_protocol_for_metrics_distribution
        t2.transport_protocol_for_metrics_distribution
    in

    let icmp_metrics_option =
      Option_utils.fusion
        Icmp_metrics.fusion
        t1.icmp_metrics_option
        t2.icmp_metrics_option
    in
    let tcp_metrics_option =
      Option_utils.fusion
        Tcp_metrics.fusion
        t1.tcp_metrics_option
        t2.tcp_metrics_option
    in
    let udp_metrics_option =
      Option_utils.fusion
        Udp_metrics.fusion
        t1.udp_metrics_option
        t2.udp_metrics_option
    in
    let ipv6_metrics_option =
      Option_utils.fusion
        Ipv6_metrics.fusion
        t1.ipv6_metrics_option
        t2.ipv6_metrics_option
    in
    let gre_metrics_option =
      Option_utils.fusion
        Gre_metrics.fusion
        t1.gre_metrics_option
        t2.gre_metrics_option
    in
    let icmpv6_metrics_option =
      Option_utils.fusion
        Icmpv6_metrics.fusion
        t1.icmpv6_metrics_option
        t2.icmpv6_metrics_option
    in
    let other_protocol_metrics_option =
      Option_utils.fusion
        Other_protocol_metrics.fusion
        t1.other_protocol_metrics_option
        t2.other_protocol_metrics_option
    in

    let r : t =
      new_t
        timestamp_sec_start
        timestamp_usec_start
        timestamp_sec_end
        timestamp_usec_end

        nb_packets
        nb_bytes
      
        packet_fingerprint

        src_addr
        dst_addr

        transport_protocol_for_metrics_distribution

        icmp_metrics_option
        tcp_metrics_option
        udp_metrics_option
        ipv6_metrics_option
        gre_metrics_option
        icmpv6_metrics_option
        other_protocol_metrics_option
    in

    if !use_verification then
      verify "" r;

    r
  )

let of_five_tuple_flow_metrics
    five_tuple_flow
    five_tuple_flow_metrics
  =
  (
    if !use_verification then
      Five_tuple_flow_metrics.verify
        (
          (Five_tuple_flow.to_string
             five_tuple_flow
          )
          ^ ": " ^
          (Five_tuple_flow_metrics.to_string
             five_tuple_flow_metrics
          )
        )
        five_tuple_flow_metrics;

    if
      (five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_start =
       five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_end)
      &&      
      (five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_start =
       five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_end)
    then
      assert(five_tuple_flow_metrics.Five_tuple_flow_metrics.nb_packets = 1);

    let nb_packets = five_tuple_flow_metrics.Five_tuple_flow_metrics.nb_packets in
    let nb_bytes = five_tuple_flow_metrics.Five_tuple_flow_metrics.nb_bytes in
    
    let packet_fingerprint_distribution =
      Packet_fingerprint_distribution.C.copy
        five_tuple_flow_metrics.Five_tuple_flow_metrics.packet_fingerprint_distribution
    in

    let src_addr =
      Ipaddr_distribution.C.of_name_value_occurence
        "src_addr"
        (Admd.Ipaddr_sb.of_ipaddr five_tuple_flow.Five_tuple_flow.src_addr)
        nb_packets
    in
    let dst_addr =
      Ipaddr_distribution.C.of_name_value_occurence
        "dst_addr"
        (Admd.Ipaddr_sb.of_ipaddr five_tuple_flow.Five_tuple_flow.dst_addr)
        nb_packets
    in

    let transport_protocol_for_metrics_distribution =
      Transport_protocol_for_metrics_distribution.C.of_name_value_occurence
        "transport_protocol_for_metrics_distribution"
        five_tuple_flow.Five_tuple_flow.protocol
        nb_packets
    in

    let src_port =
      Int_distribution.C.of_name_value_occurence
        "src_port"
        five_tuple_flow.Five_tuple_flow.src_port
        nb_packets
    in
    let dst_port =
      Int_distribution.C.of_name_value_occurence
        "dst_port"
        five_tuple_flow.Five_tuple_flow.dst_port
        nb_packets
    in

    let icmp_metrics_option =
      match five_tuple_flow_metrics.Five_tuple_flow_metrics.five_tuple_flow_transport_layer_metrics with
      | Five_tuple_flow_transport_layer_metrics.ICMP icmp_metrics ->
        Some (Icmp_metrics.copy icmp_metrics)
      | Five_tuple_flow_transport_layer_metrics.TCP _ -> None
      | Five_tuple_flow_transport_layer_metrics.UDP _ -> None
      | Five_tuple_flow_transport_layer_metrics.IPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.GRE _ -> None
      | Five_tuple_flow_transport_layer_metrics.ICMPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.Other _ -> None
    in
    let tcp_metrics_option =
      match five_tuple_flow_metrics.Five_tuple_flow_metrics.five_tuple_flow_transport_layer_metrics with
      | Five_tuple_flow_transport_layer_metrics.ICMP _ -> None
      | Five_tuple_flow_transport_layer_metrics.TCP five_tuple_flow_tcp_metrics ->
        Some
          (Tcp_metrics.new_t
             five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_tcp_packets

             (Int_distribution.C.copy src_port)
             (Int_distribution.C.copy dst_port)

             five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_urg_packets
             five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_ack_packets
             five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_psh_packets
             five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_rst_packets
             five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_syn_packets
             five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_fin_packets
          )
      | Five_tuple_flow_transport_layer_metrics.UDP _ -> None
      | Five_tuple_flow_transport_layer_metrics.IPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.GRE _ -> None
      | Five_tuple_flow_transport_layer_metrics.ICMPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.Other _ -> None
    in
    let udp_metrics_option =
      match five_tuple_flow_metrics.Five_tuple_flow_metrics.five_tuple_flow_transport_layer_metrics with
      | Five_tuple_flow_transport_layer_metrics.ICMP _ -> None
      | Five_tuple_flow_transport_layer_metrics.TCP _ -> None
      | Five_tuple_flow_transport_layer_metrics.UDP five_tuple_flow_udp_metrics ->
        Some
          (Udp_metrics.new_t
             five_tuple_flow_udp_metrics.Five_tuple_flow_udp_metrics.nb_udp_packets
             five_tuple_flow_udp_metrics.Five_tuple_flow_udp_metrics.nb_udp_datagrams

             (Int_distribution.C.copy src_port)
             (Int_distribution.C.copy dst_port)

             (Int_distribution.C.copy five_tuple_flow_udp_metrics.Five_tuple_flow_udp_metrics.data_length_distribution)
          )
      | Five_tuple_flow_transport_layer_metrics.IPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.GRE _ -> None
      | Five_tuple_flow_transport_layer_metrics.ICMPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.Other _ -> None
    in
    let ipv6_metrics_option =
      match five_tuple_flow_metrics.Five_tuple_flow_metrics.five_tuple_flow_transport_layer_metrics with
      | Five_tuple_flow_transport_layer_metrics.ICMP _ -> None
      | Five_tuple_flow_transport_layer_metrics.TCP _ -> None
      | Five_tuple_flow_transport_layer_metrics.UDP _ -> None
      | Five_tuple_flow_transport_layer_metrics.IPv6 five_tuple_flow_ipv6_metrics -> 
        Some
          (Ipv6_metrics.new_t
             five_tuple_flow_ipv6_metrics.Five_tuple_flow_ipv6_metrics.nb_packets
          )
      | Five_tuple_flow_transport_layer_metrics.GRE _ -> None
      | Five_tuple_flow_transport_layer_metrics.ICMPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.Other _ -> None
    in
    let gre_metrics_option =
      match five_tuple_flow_metrics.Five_tuple_flow_metrics.five_tuple_flow_transport_layer_metrics with
      | Five_tuple_flow_transport_layer_metrics.ICMP _ -> None
      | Five_tuple_flow_transport_layer_metrics.TCP _ -> None
      | Five_tuple_flow_transport_layer_metrics.UDP _ -> None
      | Five_tuple_flow_transport_layer_metrics.IPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.GRE five_tuple_flow_gre_metrics ->
        Some
          (Gre_metrics.new_t
             five_tuple_flow_gre_metrics.Five_tuple_flow_gre_metrics.nb_packets
          )
      | Five_tuple_flow_transport_layer_metrics.ICMPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.Other _ -> None
    in
    let icmpv6_metrics_option =
      match five_tuple_flow_metrics.Five_tuple_flow_metrics.five_tuple_flow_transport_layer_metrics with
      | Five_tuple_flow_transport_layer_metrics.ICMP _ -> None
      | Five_tuple_flow_transport_layer_metrics.TCP _ -> None
      | Five_tuple_flow_transport_layer_metrics.UDP _ -> None
      | Five_tuple_flow_transport_layer_metrics.IPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.GRE _ -> None
      | Five_tuple_flow_transport_layer_metrics.ICMPv6 icmpv6_metrics ->
        Some (Icmpv6_metrics.copy icmpv6_metrics)
      | Five_tuple_flow_transport_layer_metrics.Other _ -> None
    in
    let other_protocol_metrics_option =
      match five_tuple_flow_metrics.Five_tuple_flow_metrics.five_tuple_flow_transport_layer_metrics with
      | Five_tuple_flow_transport_layer_metrics.ICMP _ -> None
      | Five_tuple_flow_transport_layer_metrics.TCP _ -> None
      | Five_tuple_flow_transport_layer_metrics.UDP _ -> None
      | Five_tuple_flow_transport_layer_metrics.IPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.GRE _ -> None
      | Five_tuple_flow_transport_layer_metrics.ICMPv6 _ -> None
      | Five_tuple_flow_transport_layer_metrics.Other five_tuple_flow_other_protocol_metrics ->
        Some
          (Other_protocol_metrics.new_t
             five_tuple_flow_other_protocol_metrics.Five_tuple_flow_other_protocol_metrics.nb_packets
          )
    in

    let t =
      new_t
        five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_start
        five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_start
        five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_end
        five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_end

        nb_packets
        nb_bytes
      
        packet_fingerprint_distribution

        src_addr
        dst_addr

        transport_protocol_for_metrics_distribution

        icmp_metrics_option
        tcp_metrics_option
        udp_metrics_option
        ipv6_metrics_option
        gre_metrics_option
        icmpv6_metrics_option
        other_protocol_metrics_option
    in

    if !use_verification then
      verify
        (sprintf
           "five_tuple_flow_metrics:\n%s"
           (Five_tuple_flow_metrics.to_string
              five_tuple_flow_metrics
           )
        )
        t;

    (* (\* TODO: remove this when thoroughly checked *\) *)
    (* let ipaddr = five_tuple_flow.Five_tuple_flow.src_addr in *)
    (* (\* let admd_ipaddr = Admd.Ipaddr_sb.test.of_ipaddr ipaddr in *\) *)
    (* (\* let new_ipaddr = Admd.Ipaddr_sb.test.to_ipaddr admd_ipaddr in *\) *)
    (* let admd_ipaddr = Admd.Ipaddr_sb.of_ipaddr ipaddr in *)
    (* let new_ipaddr = Admd.Ipaddr_sb.to_ipaddr admd_ipaddr in *)

    (* if Ipaddr.compare ipaddr new_ipaddr <> 0 then *)
    (*   ( *)
    (*     print_endline *)
    (*       (sprintf *)
    (*          "conversion problem: %s /= %s (%s) in:\n\n%s\n%s\n\n%s" *)
    (*          (Ipaddr.to_string ipaddr) *)
    (*          (Ipaddr.to_string new_ipaddr) *)
    (*          (\* (Admd.Ipaddr_sb.test.to_string admd_ipaddr) *\) *)
    (*          (Admd.Ipaddr_sb.to_string admd_ipaddr) *)

    (*          (Five_tuple_flow.to_string five_tuple_flow) *)
    (*          (Five_tuple_flow_metrics.to_string *)
    (*             To_string_mode.Normal *)
    (*             five_tuple_flow_metrics *)
    (*          ) *)

    (*          (to_string *)
    (*             To_string_mode.Normal *)
    (*             t *)
    (*          ) *)
    (*       ) *)
    (*     ; *)
    (*     assert(false) *)
    (*   ); *)

    t
  )

let update_five_tuple_flow_metrics
    t
    five_tuple_flow
    five_tuple_flow_metrics
  =
  (
    let timestamp_sec_start = five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_start in
    let timestamp_usec_start = five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_start in
    let timestamp_sec_end = five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_sec_end in
    let timestamp_usec_end = five_tuple_flow_metrics.Five_tuple_flow_metrics.timestamp_usec_end in

    let (timestamp_sec_start, timestamp_usec_start) =
      if t.timestamp_sec_start <> timestamp_sec_start then
        (
          if t.timestamp_sec_start < timestamp_sec_start then
            (t.timestamp_sec_start, t.timestamp_usec_start)
          else
            (timestamp_sec_start, timestamp_usec_start)
        )
      else
        (t.timestamp_sec_start 
         ,
         min 
           t.timestamp_usec_start 
           timestamp_usec_start
        )
    in
    t.timestamp_sec_start <- timestamp_sec_start;
    t.timestamp_usec_start <- timestamp_usec_start;

    let (timestamp_sec_end, timestamp_usec_end) =
      if t.timestamp_sec_end <> timestamp_sec_end then
        (
          if t.timestamp_sec_end > timestamp_sec_end then
            (t.timestamp_sec_end, t.timestamp_usec_end)
          else
            (timestamp_sec_end, timestamp_usec_end)
        )
      else
        (t.timestamp_sec_end
         ,
         max
           t.timestamp_usec_end
           timestamp_usec_end
        )
    in
    t.timestamp_sec_end <- timestamp_sec_end;
    t.timestamp_usec_end <- timestamp_usec_end;

    assert(t.timestamp_sec_start <> 0);
    assert(t.timestamp_sec_end <> 0);

    let nb_packets = five_tuple_flow_metrics.Five_tuple_flow_metrics.nb_packets in
    let nb_bytes = five_tuple_flow_metrics.Five_tuple_flow_metrics.nb_bytes in

    t.nb_packets <-t.nb_packets + nb_packets;
    t.nb_bytes <- t.nb_bytes + nb_bytes;

    Packet_fingerprint_distribution.C.append
      t.packet_fingerprint_distribution
      five_tuple_flow_metrics.Five_tuple_flow_metrics.packet_fingerprint_distribution;

    Ipaddr_distribution.C.add_occurrences
      t.src_addr
      (Admd.Ipaddr_sb.of_ipaddr five_tuple_flow.Five_tuple_flow.src_addr)
      nb_packets;
    Ipaddr_distribution.C.add_occurrences
      t.dst_addr
      (Admd.Ipaddr_sb.of_ipaddr five_tuple_flow.Five_tuple_flow.dst_addr)
      nb_packets;

    Transport_protocol_for_metrics_distribution.C.add_occurrences
      t.transport_protocol_for_metrics_distribution
      five_tuple_flow.Five_tuple_flow.protocol
      nb_packets;

    let src_port =
      Int_distribution.C.of_name_value_occurence
        "src_port"
        five_tuple_flow.Five_tuple_flow.src_port
        nb_packets
    in
    let dst_port =
      Int_distribution.C.of_name_value_occurence
        "dst_port"
        five_tuple_flow.Five_tuple_flow.dst_port
        nb_packets
    in

    ignore(
      match five_tuple_flow_metrics.Five_tuple_flow_metrics.five_tuple_flow_transport_layer_metrics with
      | Five_tuple_flow_transport_layer_metrics.ICMP current_icmp_metrics ->
        (
          let new_icmp_metrics =
            match t.icmp_metrics_option with
            | None -> 
              current_icmp_metrics  
            | Some icmp_metrics -> 
              Icmp_metrics.fusion icmp_metrics current_icmp_metrics
          in

          t.icmp_metrics_option <- Some new_icmp_metrics
        )
      | Five_tuple_flow_transport_layer_metrics.TCP current_five_tuple_flow_tcp_metrics ->
        (
          let current_tcp_metrics =
            Tcp_metrics.new_t
              current_five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_tcp_packets

              src_port
              dst_port

              current_five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_urg_packets
              current_five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_ack_packets
              current_five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_psh_packets
              current_five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_rst_packets
              current_five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_syn_packets
              current_five_tuple_flow_tcp_metrics.Five_tuple_flow_tcp_metrics.nb_fin_packets
          in

          let new_tcp_metrics =
            match t.tcp_metrics_option with
            | None ->
              current_tcp_metrics
            | Some tcp_metrics ->
              Tcp_metrics.fusion tcp_metrics current_tcp_metrics
          in

          t.tcp_metrics_option <- Some new_tcp_metrics
        )
      | Five_tuple_flow_transport_layer_metrics.UDP current_five_tuple_flow_udp_metrics ->
        (
          let current_udp_metrics =
            Udp_metrics.new_t
              current_five_tuple_flow_udp_metrics.Five_tuple_flow_udp_metrics.nb_udp_packets
              current_five_tuple_flow_udp_metrics.Five_tuple_flow_udp_metrics.nb_udp_datagrams
              src_port
              dst_port
              current_five_tuple_flow_udp_metrics.Five_tuple_flow_udp_metrics.data_length_distribution
          in

          let new_udp_metrics =
            match t.udp_metrics_option with
            | None ->
              current_udp_metrics
            | Some udp_metrics ->
              Udp_metrics.fusion udp_metrics current_udp_metrics
          in

          t.udp_metrics_option <- Some new_udp_metrics
        )
      | Five_tuple_flow_transport_layer_metrics.IPv6 current_five_tuple_flow_ipv6_metrics ->
        (
          let current_ipv6_metrics =
            Ipv6_metrics.new_t
              current_five_tuple_flow_ipv6_metrics.Five_tuple_flow_ipv6_metrics.nb_packets
          in

          let new_ipv6_metrics =
            match t.ipv6_metrics_option with
            | None ->
              current_ipv6_metrics
            | Some ipv6_metrics ->
              Ipv6_metrics.fusion ipv6_metrics current_ipv6_metrics
          in

          t.ipv6_metrics_option <- Some new_ipv6_metrics
        )
      | Five_tuple_flow_transport_layer_metrics.GRE current_five_tuple_flow_gre_metrics ->
        (
          let current_gre_metrics =
            Gre_metrics.new_t
              current_five_tuple_flow_gre_metrics.Five_tuple_flow_gre_metrics.nb_packets
          in

          let new_gre_metrics =
            match t.gre_metrics_option with
            | None ->
              current_gre_metrics
            | Some gre_metrics ->
              Gre_metrics.fusion gre_metrics current_gre_metrics
          in

          t.gre_metrics_option <- Some new_gre_metrics
        )
      | Five_tuple_flow_transport_layer_metrics.ICMPv6 current_icmpv6_metrics ->
        (
          let new_icmpv6_metrics =
            match t.icmpv6_metrics_option with
            | None -> 
              current_icmpv6_metrics  
            | Some icmp_metrics -> 
              Icmpv6_metrics.fusion icmp_metrics current_icmpv6_metrics
          in

          t.icmpv6_metrics_option <- Some new_icmpv6_metrics
        )
      | Five_tuple_flow_transport_layer_metrics.Other current_five_tuple_flow_other_protocol_metrics ->
          let current_other_protocol_metrics =
            Other_protocol_metrics.new_t
              current_five_tuple_flow_other_protocol_metrics.Five_tuple_flow_other_protocol_metrics.nb_packets
          in

          let new_other_protocol_metrics =
            match t.other_protocol_metrics_option with
            | None ->
              current_other_protocol_metrics
            | Some other_protocol_metrics ->
              Other_protocol_metrics.fusion
                other_protocol_metrics
                current_other_protocol_metrics
          in

          t.other_protocol_metrics_option <- Some new_other_protocol_metrics        
    );

    if !use_verification then
      verify "" t;
  )

let update_packet_data_for_metrics
    t
    packet_data_for_metrics
  =
  (
    let timestamp_sec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec in
    let timestamp_usec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_usec in

    let (timestamp_sec_start, timestamp_usec_start) =
      if t.timestamp_sec_start <> timestamp_sec then
        (
          if t.timestamp_sec_start < timestamp_sec then
            (t.timestamp_sec_start, t.timestamp_usec_start)
          else
            (timestamp_sec, timestamp_usec)
        )
      else
        (t.timestamp_sec_start 
         ,
         min 
           t.timestamp_usec_start 
           timestamp_usec
        )
    in
    t.timestamp_sec_start <- timestamp_sec_start;
    t.timestamp_usec_start <- timestamp_usec_start;

    let (timestamp_sec_end, timestamp_usec_end) =
      if t.timestamp_sec_end <> timestamp_sec then
        (
          if t.timestamp_sec_end > timestamp_sec then
            (t.timestamp_sec_end, t.timestamp_usec_end)
          else
            (timestamp_sec, timestamp_usec)
        )
      else
        (t.timestamp_sec_end
         ,
         max
           t.timestamp_usec_end
           timestamp_usec
        )
    in
    t.timestamp_sec_end <- timestamp_sec_end;
    t.timestamp_usec_end <- timestamp_usec_end;

    t.nb_packets <- t.nb_packets + 1;

    t.nb_bytes <- t.nb_bytes + packet_data_for_metrics.Packet_data_for_metrics.length;

    let packet_fingerprint =
      Packet_fingerprint.of_packet_data_for_metrics
        packet_data_for_metrics
    in
    Packet_fingerprint_distribution.C.add_single_occurrence
      t.packet_fingerprint_distribution
      packet_fingerprint;

    ignore(
      match packet_data_for_metrics.Packet_data_for_metrics.pdu_t with
      | Packet_data_for_metrics.IPV4 ipv4_data_for_metrics ->
        (
          Ipaddr_distribution.C.add_single_occurrence
            t.src_addr
            (Admd.Ipaddr_sb.of_ipaddrv4 ipv4_data_for_metrics.Ipv4_data_for_metrics.source_address);
          Ipaddr_distribution.C.add_single_occurrence
            t.dst_addr
            (Admd.Ipaddr_sb.of_ipaddrv4 ipv4_data_for_metrics.Ipv4_data_for_metrics.destination_address);

          let fragmentation, first_fragment =
            Ipv4_data_for_metrics.get_fragmentation_boolean
              ipv4_data_for_metrics
          in

          match ipv4_data_for_metrics.Ipv4_data_for_metrics.pdu_t with
          | Ipv4_data_for_metrics.ICMP icmp_data_for_metrics ->
            (
              let icmp_metrics = 
                match t.icmp_metrics_option with
                | None -> 
                  Icmp_metrics.new_single_t ()
                | Some icmp_metrics -> 
                  icmp_metrics
              in

              Icmp_metrics.update_icmp_data_for_metrics icmp_metrics icmp_data_for_metrics;

              t.icmp_metrics_option <- Some icmp_metrics;        
            )
          | Ipv4_data_for_metrics.TCP tcp_for_metrics ->
            (
              let tcp_metrics = 
                match t.tcp_metrics_option with
                | None ->
                  Tcp_metrics.new_empty_t ()
                | Some tcp_metrics ->
                  tcp_metrics
              in

              Tcp_metrics.update tcp_metrics tcp_for_metrics;

              t.tcp_metrics_option <- Some tcp_metrics;
            )
          | Ipv4_data_for_metrics.UDP udp_for_metrics ->
            (
              let udp_metrics = 
                match t.udp_metrics_option with
                | None -> 
                  Udp_metrics.new_single_t ()
                | Some udp_metrics -> udp_metrics
              in

              Udp_metrics.update
                udp_metrics
                fragmentation
                first_fragment
                udp_for_metrics;

              t.udp_metrics_option <- Some udp_metrics;
            )
          | Ipv4_data_for_metrics.IPv6 ipv6_for_metrics ->
            (
              let ipv6_metrics = 
                match t.ipv6_metrics_option with
                | None -> 
                  Ipv6_metrics.new_empty_t ()
                | Some ipv6_metrics -> ipv6_metrics
              in

              Ipv6_metrics.update ipv6_metrics ipv6_for_metrics;

              t.ipv6_metrics_option <- Some ipv6_metrics;
            )
          | Ipv4_data_for_metrics.GRE ->
            (
              let gre_metrics = 
                match t.gre_metrics_option with
                | None -> 
                  Gre_metrics.new_empty_t ()
                | Some gre_metrics -> gre_metrics
              in

              Gre_metrics.update gre_metrics;

              t.gre_metrics_option <- Some gre_metrics;
            )
          | Ipv4_data_for_metrics.Other other_protocol_metrics ->
            let other_protocol_metrics = 
              match t.other_protocol_metrics_option with
              | None -> 
                Other_protocol_metrics.new_empty_t ()
              | Some other_protocol_metrics -> other_protocol_metrics
            in

            Other_protocol_metrics.update other_protocol_metrics;

            t.other_protocol_metrics_option <- Some other_protocol_metrics;
        )
      | Packet_data_for_metrics.IPV6 ipv6_for_metrics ->
        (
          (* Int32_distribution.add_single_occurence t.src_addr ipv6_for_metrics.Ipv6_data_for_metrics.source_address; *)
          (* Int32_distribution.add_single_occurence t.dst_addr ipv6_for_metrics.Ipv6_data_for_metrics.destination_address; *)

          (* match ipv6_for_metrics.Ipv6_data_for_metrics.pdu_t with *)
          (* | Ipv6_data_for_metrics.ICMP icmp_data_for_metrics -> *)
          (*   ( *)
          (*     let icmp_metrics =  *)
          (*       match t.icmp_metrics_option with *)
          (*       | None ->  *)
          (*         Icmp_metrics.new_empty_t () *)
          (*       | Some icmp_metrics ->  *)
          (*         icmp_metrics *)
          (*     in *)

          (*     Icmp_metrics.update_icmp_data_for_metrics icmp_metrics icmp_data_for_metrics; *)

          (*     t.icmp_metrics_option <- Some icmp_metrics;         *)
          (*   ) *)
          (* | Ipv6_data_for_metrics.TCP tcp_for_metrics -> *)
          (*   ( *)
          (*     let tcp_metrics =  *)
          (*       match t.tcp_metrics_option with *)
          (*       | None -> *)
          (*         Tcp_metrics.new_empty_t () *)
          (*       | Some tcp_metrics -> *)
          (*         tcp_metrics *)
          (*     in *)

          (*     Tcp_metrics.update tcp_metrics tcp_for_metrics; *)

          (*     t.tcp_metrics_option <- Some tcp_metrics; *)
          (*   ) *)
          (* | Ipv6_data_for_metrics.UDP udp_for_metrics -> *)
          (*   ( *)
          (*     let udp_metrics =  *)
          (*       match t.udp_metrics_option with *)
          (*       | None ->  *)
          (*         Udp_metrics.new_empty_t () *)
          (*       | Some udp_metrics -> udp_metrics *)
          (*     in *)

          (*     Udp_metrics.update udp_metrics udp_for_metrics; *)

          (*     t.udp_metrics_option <- Some udp_metrics; *)
          (*   ) *)
          (* | Ipv6_data_for_metrics.Other protocol_number -> () *)

          ()
        )
      | Packet_data_for_metrics.Other -> ()
    );

    if !use_verification then
      verify "" t;

    failwith "[Detailed_metrics]: update_packet_data_for_metrics: DO NOT USE"
  )

let to_indice_name_tuple_array _ =
  [|
    (0 , "src_port");
    (1 , "dst_port")
  |]


let get_src_dst_port_distribution
    t
  =
  match t.udp_metrics_option with
  | None -> 
    (
      match t.tcp_metrics_option with
      | None -> 
        (Int_distribution.C.new_empty_t (),
         Int_distribution.C.new_empty_t ())
      | Some tcp_metrics ->
        (tcp_metrics.Tcp_metrics.src_port,
         tcp_metrics.Tcp_metrics.dst_port)
    )
  | Some udp_metrics ->
    (
      match t.tcp_metrics_option with
      | None ->
        (udp_metrics.Udp_metrics.src_port
        , udp_metrics.Udp_metrics.dst_port)
      | Some tcp_metrics ->
        (
          (Int_distribution.C.fusion
             udp_metrics.Udp_metrics.src_port
             tcp_metrics.Tcp_metrics.src_port
          )
          ,
          (Int_distribution.C.fusion
             udp_metrics.Udp_metrics.dst_port
             tcp_metrics.Tcp_metrics.dst_port)
        )
    )
    

