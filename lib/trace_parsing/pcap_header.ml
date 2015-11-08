
type t =
  {
    ts: Pcap_timeval.t;
    caplen: int;
    len: int;
  }

let new_t
    ts
    caplen
    len
  =
  {
    ts;
    caplen;
    len;
  }

let copy t =
  new_t
    (Pcap_timeval.copy t.ts)
    t.caplen
    t.len
