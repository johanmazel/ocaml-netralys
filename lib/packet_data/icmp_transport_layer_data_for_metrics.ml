
open Printf

open Ipv4
open Tcp
open Udp
open Icmp

type t =
| ICMP of Icmp_icmp_data_for_metrics.t
| TCP of Icmp_tcp_data_for_metrics.t
| UDP of Udp_data_for_metrics.t

let of_melange icmp_ip_packet =  
  try
    (
      match icmp_ip_packet#protocol with
      | `ICMP ->
        (        
          let icmp_icmp = Icmp.unmarshal icmp_ip_packet#data_env in

          let icmp_icmp_data_for_metrics = Icmp_icmp_data_for_metrics.of_melange icmp_icmp in

          ICMP icmp_icmp_data_for_metrics
        )
      | (`TCP : Ipv4.protocol_t) ->
        (        
          let icmp_tcp = Tcp.unmarshal icmp_ip_packet#data_env in

          let icmp_tcp_data_for_metrics = Icmp_tcp_data_for_metrics.of_melange_tcp icmp_tcp in

          TCP icmp_tcp_data_for_metrics
        )
      | (`UDP : Ipv4.protocol_t) ->
        (
          let icmp_udp = Udp.unmarshal icmp_ip_packet#data_env in

          let udp_data_for_metrics =
            Udp_data_for_metrics.of_melange_udp
              icmp_udp
          in            
          UDP udp_data_for_metrics
        )
      | `IGMP ->
        (
          assert(false);
        )
      |
        `Unknown value ->
        (
          assert(false);
        )
    )
  with
  | exn -> assert(false);
        
