
type pdu_t =
| ICMP of Icmp_data_for_metrics.t
| TCP of Tcp_data_for_metrics.t
| UDP of Udp_data_for_metrics.t
| GRE
| ICMP6 of Icmp6_data_for_metrics.t
| Other of int

type t =
  {
    source_address : Ipaddr.V6.t;
    destination_address : Ipaddr.V6.t;
    pdu_t: pdu_t;
  }

let new_t
    source_address
    destination_address
    pdu_t
  =
  {
    source_address;
    destination_address;
    pdu_t;
  }
