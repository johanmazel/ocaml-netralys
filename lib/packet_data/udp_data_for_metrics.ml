
type t =
  {
    source_port : int;
    destination_port : int;
    data_length : int;
  }

let new_t
    source_port
    destination_port
    data_length
  =
  {
    source_port;
    destination_port;
    data_length;
  }

let of_melange_udp udp =
  let source_port = udp#source_port in
  let destination_port = udp#dest_port in
  let data_length = udp#data_length in
  assert(data_length > 0);
  let udp_for_metrics =
    new_t
      source_port
      destination_port
      data_length
  in

  udp_for_metrics
