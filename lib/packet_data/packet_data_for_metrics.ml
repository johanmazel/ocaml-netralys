
open Printf

type pdu_t =
| IPV4 of Ipv4_data_for_metrics.t
| IPV6 of Ipv6_data_for_metrics.t
| Other

type t =
  {
    timestamp_sec : int;
    timestamp_usec : int;
    length : int;
    pdu_t: pdu_t;
  }

let new_t
    timestamp_sec
    timestamp_usec
    length
    pdu_t
  =
  {
    timestamp_sec = timestamp_sec;
    timestamp_usec = timestamp_usec;
    length = length;
    pdu_t = pdu_t;
  }

let pdu_t_of_ipv4
    ipv4_pdu
  =
  try
    (
      match ipv4_pdu#protocol with
      | `ICMP ->
        (
          let icmp = Icmp.Icmp.unmarshal ipv4_pdu#data_env in

          (* The 0 value refers to the first call to this function. It is used
              to trace the number of level encpasulated ICMP and not go futher than
              one. *)
          let icmp_data_for_metrics = Icmp_data_for_metrics.of_melange icmp in

          Ipv4_data_for_metrics.ICMP icmp_data_for_metrics
        )
      | (`TCP : Ipv4.Ipv4.protocol_t) ->
        (
          let tcp = Tcp.Tcp.unmarshal ipv4_pdu#data_env in

          let tcp_data_for_metrics =
            Tcp_data_for_metrics.of_melange_tcp
              tcp
          in

          Ipv4_data_for_metrics.TCP tcp_data_for_metrics
        )
      | (`UDP : Ipv4.Ipv4.protocol_t) ->
        (
          let udp = Udp.Udp.unmarshal ipv4_pdu#data_env in

          let udp_for_metrics = Udp_data_for_metrics.of_melange_udp udp in

          Ipv4_data_for_metrics.UDP udp_for_metrics
        )
      | `IGMP ->
        (
          Ipv4_data_for_metrics.Other 2
        )
      | `Unknown protocol_code ->
        (
          match protocol_code with
          | 41 ->
            let ipv6_for_metrics = 
              Ipv6_data_for_metrics.new_t 
                (Ipaddr.V6.of_int64 ((Int64.of_int 0), (Int64.of_int 0)))
                (Ipaddr.V6.of_int64 ((Int64.of_int 0), (Int64.of_int 0)))
                (Ipv6_data_for_metrics.Other 0)
            in
            Ipv4_data_for_metrics.IPv6 ipv6_for_metrics
          | 47 ->
            Ipv4_data_for_metrics.GRE
          | _ ->
            Ipv4_data_for_metrics.Other (-1)
        )
    )
  with
  | exn -> Ipv4_data_for_metrics.Other (-2)


let env = Mpl_stdlib.new_env (String.make 2000 '\000');;

let pdu_t_of_ipv6
    caplen
    ip6_pdu
  =
  let ipv6_data_for_metrics_pdu_t =
    match ip6_pdu.Ip6.Pdu.proto with
    | 1 ->
      (
        let icmp = Icmp.Icmp.unmarshal env in
        let icmp_data_for_metrics = Icmp_data_for_metrics.of_melange icmp in         
        Ipv6_data_for_metrics.ICMP icmp_data_for_metrics
      )
    | 6 ->
      (
        let tcp = Tcp.Tcp.unmarshal env in         
        let tcp_for_metrics = Tcp_data_for_metrics.of_melange_tcp tcp in         
        Ipv6_data_for_metrics.TCP tcp_for_metrics
      )
    | 17 ->
      (
        let udp = Udp.Udp.unmarshal env in         
        let udp_for_metrics = Udp_data_for_metrics.of_melange_udp udp in
        Ipv6_data_for_metrics.UDP udp_for_metrics
      )
    | 47 ->
      Ipv6_data_for_metrics.GRE
    | 58 ->
      (
        let icmp6 = Icmp6.Icmp6.unmarshal env in
        let icmp6_data_for_metrics = Icmp6_data_for_metrics.of_melange_icmpv6 icmp6 in

        (* let string = Mpl_stdlib.string_of_env env in *)

        (* let bitstring = Bitstring.bitstring_of_string string in *)

        (* let error_string = *)
        (*   "" *)
        (*   (\* sprintf *\) *)
        (*   (\*   "%d,%d" *\) *)
        (*   (\*   timestamp_sec *\) *)
        (*   (\*   timestamp_usec *\) *)
        (* in *)

        (* let bitstring = Bitstring.bitstring_of_string string in *)

        (* let ethernet_header_length = 14 in *)
        (* let ipv6_header_length = 40 in *)
        (* let icmpv6_header_length = 4 in *)
        (* let data_length = *)
        (*   caplen - ethernet_header_length - ipv6_header_length - icmpv6_header_length *)
        (* in               *)
        (* let icmpv6_pdu_option =  *)
        (*   Icmpv6.unpack *)
        (*     error_string *)
        (*     data_length *)
        (*     bitstring *)
        (* in *)

        (* let icmpv6_data_for_metrics = Icmpv6_data_for_metrics.of_melange_icmpv6 icmp in *)

        Ipv6_data_for_metrics.ICMP6 icmp6_data_for_metrics
      )
    | _ ->
      Ipv6_data_for_metrics.Other (-1)
  in

  ipv6_data_for_metrics_pdu_t

let of_melange_ethernet
    packet_parsing_mode
    (pcap_header : Pcap_header.t)
    (ethernet_pdu : Ethernet.Ethernet.o)
  =
  let timestamp_sec = pcap_header.Pcap_header.ts.Pcap_timeval.tv_sec in
  let timestamp_usec = pcap_header.Pcap_header.ts.Pcap_timeval.tv_usec in

  (* let caplen = pcap_header.Pcap_types.caplen in *)
  let length = pcap_header.Pcap_header.len in

  (* print_endline *)
  (*   (sprintf *)
  (*      "[Packet_data_for_metrics]: caplen: %d ; len: %d" *)
  (*      caplen *)
  (*      length *)
  (*   ); *)

  let pdu_t =
    try
      (
        match ethernet_pdu with
        |`E802_2 ether_E802_2_packet ->
          (
            Other
          )
        | `IPv4 ether_IPv4_packet ->
          (
            if Packet_parsing_mode.process_ipv4 packet_parsing_mode then
              try
                (
                  let ipv4_pdu = Ipv4.Ipv4.unmarshal ether_IPv4_packet#data_env in

                  let source_address = ipv4_pdu#src in
                  let destination_address = ipv4_pdu#dest in

                  let ip_id = ipv4_pdu#id in

                  let more_fragment =
                    match  ipv4_pdu#can_fragment with
                    | 0 -> false
                    | 1 -> true
                    | _ -> failwith "IP packet is a quantum "
                  in
                  let fragment_offset = ipv4_pdu#frag_offset in

                  let ipv4_data_for_metrics_pdu_t =
                    pdu_t_of_ipv4
                      ipv4_pdu
                  in

                  IPV4
                    (Ipv4_data_for_metrics.new_t
                       (Ipaddr.V4.of_int32 source_address)
                       (Ipaddr.V4.of_int32 destination_address)
                       ip_id
                       more_fragment
                       fragment_offset
                       ipv4_data_for_metrics_pdu_t
                    )
                )
              with
              | Ipv4.Ipv4.Bad_packet message_string -> Other
            else
              Other
          )                                         
        |`Arp ether_Arp_packet ->
          Other
        |`IPv6 ether_IPv6packet ->
          (
            if Packet_parsing_mode.process_ipv6 packet_parsing_mode then
              let string = Mpl_stdlib.string_of_env ether_IPv6packet#data_env in

              (* print_endline ""; *)
              (* print_endline *)
              (*          (sprintf *)
              (*             "Packet_data_for_metrics: of_melange_ethernet: string: %s" *)
              (*             string *)
              (*          ); *)

              let error_string =
                sprintf
                  "%d,%d"
                  timestamp_sec
                  timestamp_usec
              in

              let bitstring = Bitstring.bitstring_of_string string in

              let ethernet_header_length = 14 in
              let ipv6_header_length = 40 in
              let data_length =
                pcap_header.Pcap_header.caplen - ethernet_header_length - ipv6_header_length
              in              
              let ip6_pdu_option = 
                Ip6.Pdu.unpack
                  error_string
                  data_length
                  bitstring
              in

              match ip6_pdu_option with
              | None ->
                Other
              | Some ip6_pdu ->
                (
                  let payload_string = Bitstring.string_of_bitstring ip6_pdu.Ip6.Pdu.payload in

                  (* print_endline *)
                  (*   (sprintf *)
                  (*          "Packet_data_for_metrics: of_melange_ethernet: payload_string: %s" *)
                  (*          payload_string *)
                  (*   ); *)

                  Mpl_stdlib.reset env;
                  Mpl_stdlib.fill_string env payload_string;

                  let src_ipaddr = Ipaddr.V6.of_string_exn (Unix.string_of_inet_addr (Obj.magic ip6_pdu.Ip6.Pdu.src)) in
                  let dst_ipaddr = Ipaddr.V6.of_string_exn (Unix.string_of_inet_addr (Obj.magic ip6_pdu.Ip6.Pdu.dst)) in

                  let ipv6_data_for_metrics_pdu_t : Ipv6_data_for_metrics.pdu_t =
                    pdu_t_of_ipv6
                      pcap_header.Pcap_header.caplen
                      ip6_pdu
                  in

                  IPV6
                    (Ipv6_data_for_metrics.new_t
                       src_ipaddr
                       dst_ipaddr
                       ipv6_data_for_metrics_pdu_t
                    )

                )
            else
              Other
          )
      )
    with
    | Ethernet.Ethernet.Bad_packet message_string ->
      (
        Other
      )
    | exn ->
      Other
  in

  new_t
    timestamp_sec
    timestamp_usec
    length
    pdu_t
