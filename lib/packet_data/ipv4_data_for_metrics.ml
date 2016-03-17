
type pdu_t =
  | ICMP of Icmp_data_for_metrics.t
  | TCP of Tcp_data_for_metrics.t
  | UDP of Udp_data_for_metrics.t
  | IPv6 of Ipv6_data_for_metrics.t
  | GRE
  | Other of int

type t =
  {
    source_address : Ipaddr.V4.t;
    destination_address : Ipaddr.V4.t;
    ip_id : int;
    more_fragment : bool;
    fragment_offset : int;
    pdu_t: pdu_t;
  }

let new_t
    source_address
    destination_address
    ip_id
    more_fragment
    fragment_offset
    pdu_t
  =
  {
    source_address;
    destination_address;
    ip_id;
    more_fragment;
    fragment_offset;
    pdu_t;
  }

let get_fragmentation_boolean t =
  let fragmentation =
    t.more_fragment
  in
  let first_fragment =
    if t.fragment_offset == 0 then
      true
    else
      false
  in
  fragmentation, first_fragment
