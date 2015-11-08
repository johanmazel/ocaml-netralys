
type t =
  {
    source_port : int;
    destination_port : int;

    seq_number : int;

    urg : bool;
    ack : bool;
    psh : bool;
    rst : bool;
    syn : bool;
    fin : bool;
  }

let new_t
    source_port
    destination_port

    seq_number

    urg
    ack
    psh
    rst
    syn
    fin
  =
  {
    source_port;
    destination_port;

    seq_number;

    urg;
    ack;
    psh;
    rst;
    syn;
    fin;
  }

let of_melange_tcp (tcp : Tcp.Tcp.o) =
  let source_port = tcp#sport in
  let destination_port = tcp#dport in
  
  let seq_number = tcp#seq_number in
    
  let urg =
    match tcp#urg with
    | 0 -> false
    | 1 -> true
    | _ -> failwith "TCP packet is a quantum URG"
  in

  let ack =
    match tcp#ack with
    | 0 -> false
    | 1 -> true
    | _ -> failwith "TCP packet is a quantum ACK"
  in

  let psh =
    match tcp#psh with
    | 0 -> false
    | 1 -> true
    | _ -> failwith "TCP packet is a quantum PSH"
  in

  let rst =
    match tcp#rst with
    | 0 -> false
    | 1 -> true
    | _ -> failwith "TCP packet is a quantum RST"
  in

  let syn =
    match tcp#syn with
    | 0 -> false
    | 1 -> true
    | _ -> failwith "TCP packet is a quantum SYN"
  in
  
  let fin =
    match tcp#fin with
    | 0 -> false
    | 1 -> true
    | _ -> failwith "TCP packet is a quantum FIN"
  in
  
  let tcp_for_metrics =
    new_t
      source_port
      destination_port

      (Int32.to_int seq_number)
      
      urg
      ack
      psh
      rst
      syn
      fin
  in

  tcp_for_metrics
