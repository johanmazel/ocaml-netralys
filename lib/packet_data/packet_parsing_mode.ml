
open Printf

type t =
| IPV4
| IPV6
| IPV46

let of_string string =
  (* print_endline *)
  (*   (sprintf *)
  (*      "Packet_parsing_mode: of_string: %s" *)
  (*      string *)
  (*   ); *)
  match string with
  | "4" -> IPV4
  | "6" -> IPV6
  | "46" -> IPV46
  | "64" -> IPV46
  | _ -> failwith "Invalid string for packet parsing \"%s\" expected \"4|6|46|64\""

let process_ipv4 t =
  match t with
  | IPV4 -> true
  | IPV6 -> false
  | IPV46 -> true

let process_ipv6 t =
  match t with
  | IPV4 -> false
  | IPV6 -> true
  | IPV46 -> true

let to_string t =
  match t with
  | IPV4 -> "IPV4"
  | IPV6 -> "IPV6"
  | IPV46 -> "IPV46"
