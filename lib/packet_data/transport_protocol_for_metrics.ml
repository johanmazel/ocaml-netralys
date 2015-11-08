
open Printf

open Sexplib.Std
open Bin_prot.Std

type t =
| ICMP
| TCP
| UDP
| IPv6
| GRE
| ICMPv6
| Other of int
with compare, sexp, bin_io

let of_string string =
  match string with
  | "icmp" -> ICMP
  | "tcp" -> TCP
  | "udp" -> UDP
  | "ipv6" -> IPv6
  | "gre" -> GRE
  | "icmpv6" -> ICMPv6
  | _ -> 
    (* Other string *)
    failwith (sprintf "Transport_protocol: of_string: invalid string: %s" string)

let to_string string =
  match string with
  (* | None -> "" *)
  | ICMP -> "icmp"
  | TCP -> "tcp"
  | UDP -> "udp"
  | IPv6 -> "ipv6"
  | GRE -> "gre"
  | ICMPv6 -> "icmpv6"
  | Other int -> string_of_int int

let of_int int =
  match int with
  | 1 -> ICMP
  | 6 -> TCP
  | 17 -> UDP
  | 41 -> IPv6
  | 47 -> GRE
  | 58 -> ICMPv6
  | _ -> 
    (* | _ -> Other "" *)
    failwith (sprintf "Transport_protocol: of_string: invalid int: %d" int)

let to_int t =
  match t with
  (* | None -> -1 *)
  | ICMP -> 1
  | TCP -> 6
  | UDP -> 17
  | IPv6 -> 41
  | GRE -> 47
  | ICMPv6 -> 58
  | Other int -> int

let equal t1 t2 = Batteries.Int.equal 0 (compare t1 t2)

let to_admd_transport_protocol
    t
    =
  match t with
  | ICMP -> Admd.Transport_protocol.ICMP
  | TCP -> Admd.Transport_protocol.TCP
  | UDP -> Admd.Transport_protocol.UDP
  | IPv6 -> Admd.Transport_protocol.IPv6
  | GRE -> Admd.Transport_protocol.GRE
  | ICMPv6 -> Admd.Transport_protocol.ICMPv6
  | Other code -> Admd.Transport_protocol.None
  
