
type t =
  {
    source_port : int;
    destination_port : int;
    syn : bool;
    rst : bool;
    fin : bool;
  }

let new_t
    source_port
    destination_port
    syn
    rst
    fin
    =
  {
    source_port = source_port;
    destination_port = destination_port;
    syn = syn;
    rst = rst;
    fin = fin;
  }

let of_melange_tcp tcp =
  let source_port = tcp#sport in
  let destination_port = tcp#dport in
  
  let syn =
    match tcp#syn with
    | 0 -> false
    | 1 -> true
    | _ -> failwith "TCP packet is a quantic SYN"
  in
  
  let rst =
    match tcp#rst with
    | 0 -> false
    | 1 -> true
    | _ -> failwith "TCP packet is a quantic RST"
  in

  let fin =
    match tcp#fin with
    | 0 -> false
    | 1 -> true
    | _ -> failwith "TCP packet is a quantic FIN"
  in
  
  let tcp_for_metrics =
    new_t
      source_port
      destination_port
      syn
      rst
      fin
  in

  tcp_for_metrics
