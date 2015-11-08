
(* open Ipv4 *)
(* open Tcp *)
(* open Udp *)
(* open Icmp *)

type t =
| Destination_unreachable of (int * Ipaddr.t * Ipaddr.t * Icmp_transport_layer_data_for_metrics.t)
| Time_exceeded of (int * Ipaddr.t * Ipaddr.t * Icmp_transport_layer_data_for_metrics.t)
             
| Echo_request         
| Echo_reply
| Router_solicitation
| Router_advertisement
| Other

let of_melange_icmpv6 (icmp6 : Icmp6.Icmp6.o) =
  match icmp6 with
  | `DestinationUnreachable destination_unreachable ->
    (
      let code = destination_unreachable#code in

      let icmp_ip = destination_unreachable#ip6_header_env in

      let icmp_ip_packet = Ipv4.Ipv4.unmarshal icmp_ip in

      let icmp_du_source_address = icmp_ip_packet#src in
      let icmp_du_destination_address = icmp_ip_packet#dest in

      let icmp_transport_layer_data_for_metrics = Icmp_transport_layer_data_for_metrics.of_melange
          icmp_ip_packet
      in

      Destination_unreachable 
        (code, (Ipaddr.V4 (Ipaddr.V4.of_int32 icmp_du_source_address)), (Ipaddr.V4 (Ipaddr.V4.of_int32 icmp_du_destination_address)),
         icmp_transport_layer_data_for_metrics)
    )
  | `TimeExceeded time_exceeded -> 
    (
      let code = time_exceeded#code in

      let icmp_ip = time_exceeded#ip6_header_env in

      let icmp_ip_packet = Ipv4.Ipv4.unmarshal icmp_ip in

      let icmp_du_source_address = icmp_ip_packet#src in
      let icmp_du_destination_address = icmp_ip_packet#dest in

      let icmp_transport_layer_data_for_metrics = Icmp_transport_layer_data_for_metrics.of_melange
          icmp_ip_packet
      in

      Time_exceeded
        (code, (Ipaddr.V4 (Ipaddr.V4.of_int32 icmp_du_source_address)), (Ipaddr.V4 (Ipaddr.V4.of_int32 icmp_du_destination_address)),
         icmp_transport_layer_data_for_metrics)
    )

  | `EchoRequest echo_request -> Echo_request
  | `EchoReply echo_reply ->
    Echo_reply

  | _ -> Other

