
open Ipv4
open Tcp
open Udp
open Icmp

type t =
| Echo_reply
(* | Destination_unreachable of (int * int32 * int32 * Icmp_transport_layer_data_for_metrics.t) *)
| Destination_unreachable of (int * Ipaddr.t * Ipaddr.t * Icmp_transport_layer_data_for_metrics.t)
| Source_quench
(* | Redirect_message of (int * int32 * int32 * Icmp_transport_layer_data_for_metrics.t) *)
| Redirect_message of (int * Ipaddr.t * Ipaddr.t * Icmp_transport_layer_data_for_metrics.t)
| Echo_request
(* | Time_exceeded of (int * int32 * int32 * Icmp_transport_layer_data_for_metrics.t) *)
| Time_exceeded of (int * Ipaddr.t * Ipaddr.t * Icmp_transport_layer_data_for_metrics.t)
| Timestamp_request
| Timestamp_reply
| Netmask_request
| Netmask_reply
| Router_advertisement
| Router_solicitation
| Other

(* type t =                         *)
(*   {                              *)
(*     echo_request : bool;         *)
(*     echo_reply : bool;           *)
(*   }                              *)

(* let new_t                        *)
(*     echo_request                 *)
(*     echo_reply                   *)
(* =                                *)
(*   {                              *)
(*     echo_request = echo_request; *)
(*     echo_reply = echo_reply;     *)
(*   }                              *)

let of_melange icmp =
  match icmp with
  | `EchoReply echo_reply ->
    Echo_reply
  | `DestinationUnreachable destination_unreachable ->
    (
      let code = destination_unreachable#code in

      let icmp_ip = destination_unreachable#ip_header_env in
      
      let icmp_ip_packet = Ipv4.unmarshal icmp_ip in
      
      let icmp_du_source_address = icmp_ip_packet#src in
      let icmp_du_destination_address = icmp_ip_packet#dest in
      
      let icmp_transport_layer_data_for_metrics = Icmp_transport_layer_data_for_metrics.of_melange
  icmp_ip_packet
      in

      Destination_unreachable 
  (code, (Ipaddr.V4 (Ipaddr.V4.of_int32 icmp_du_source_address)), (Ipaddr.V4 (Ipaddr.V4.of_int32 icmp_du_destination_address)),
   icmp_transport_layer_data_for_metrics)
    )
  | `SourceQuench _ -> Source_quench
  | `Redirect redirect -> 
    (
      let code = redirect#code in

      let icmp_ip = redirect#ip_header_env in

      let icmp_ip_packet = Ipv4.unmarshal icmp_ip in

      let icmp_du_source_address = icmp_ip_packet#src in
      let icmp_du_destination_address = icmp_ip_packet#dest in
      
      let icmp_transport_layer_data_for_metrics = Icmp_transport_layer_data_for_metrics.of_melange
  icmp_ip_packet
      in

      Redirect_message
  (code, (Ipaddr.V4 (Ipaddr.V4.of_int32 icmp_du_source_address)), (Ipaddr.V4 (Ipaddr.V4.of_int32 icmp_du_destination_address)),
   icmp_transport_layer_data_for_metrics)
    )
  | `EchoRequest echo_request -> Echo_request
  | `TimeExceeded time_exceeded -> 
    (
      let code = time_exceeded#code in

      let icmp_ip = time_exceeded#ip_header_env in

      let icmp_ip_packet = Ipv4.unmarshal icmp_ip in

      let icmp_du_source_address = icmp_ip_packet#src in
      let icmp_du_destination_address = icmp_ip_packet#dest in
      
      let icmp_transport_layer_data_for_metrics = Icmp_transport_layer_data_for_metrics.of_melange
  icmp_ip_packet
      in

      Time_exceeded
  (code, (Ipaddr.V4 (Ipaddr.V4.of_int32 icmp_du_source_address)), (Ipaddr.V4 (Ipaddr.V4.of_int32 icmp_du_destination_address)),
   icmp_transport_layer_data_for_metrics)
    )
  | `TimestampRequest timestamp_request -> Timestamp_request
  | `TimestampReply timestamp_reply -> Timestamp_reply
  | `NetmaskRequest netmask_request -> Netmask_request
  | `NetmaskReply netmask_reply -> Netmask_reply
  | `RouterAdvertisement _ -> Router_advertisement
  | `RouterSolicitation _ -> Router_solicitation
  | _ -> Other

