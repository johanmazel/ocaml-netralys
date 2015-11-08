
open Printf

let transport_protocol_for_metrics_to_admd_transport_protocol
    transport_protocol_for_metrics
    =
  match transport_protocol_for_metrics with
  | Transport_protocol_for_metrics.ICMP -> Admd.Admd_transport_protocol.ICMP
  | Transport_protocol_for_metrics.TCP -> Admd.Admd_transport_protocol.TCP
  | Transport_protocol_for_metrics.UDP -> Admd.Admd_transport_protocol.UDP
  | Transport_protocol_for_metrics.IPv6 -> Admd.Admd_transport_protocol.IPv6
  | Transport_protocol_for_metrics.GRE -> Admd.Admd_transport_protocol.GRE
  | Transport_protocol_for_metrics.ICMPv6 -> Admd.Admd_transport_protocol.ICMPv6
  | Transport_protocol_for_metrics.Other code -> Admd.Admd_transport_protocol.None
  
