
open Printf

let open_file filename =
  let fd = Unix.(openfile filename [O_RDONLY] 0) in
  let bigarray = Bigarray.(Array1.map_file fd Bigarray.char c_layout false (-1)) in
  Cstruct.of_bigarray bigarray

let launch_analysis
    filename
    (function_to_apply : Pcap_header.t -> string -> unit)
  =
  (
    let buf = open_file filename in

    let h =
      match Pcap.detect buf with
      | Some header -> header
      | None ->  failwith (Printf.sprintf "failed to parse pcap header from %s" filename)
    in

    let header, body = Cstruct.split buf Pcap.sizeof_pcap_header in

    let buf_tuple_iter = Pcap.packets h body in

    let module H = (val h: Pcap.HDR) in

    Cstruct.fold
      (fun _ (header , packet) ->
         (* let caplen = H.get_pcap_packet_incl_len header in *)
         (* let snaplen = H.get_pcap_packet_orig_len header in *)

         let caplen = H.get_pcap_packet_incl_len header in
         let len = H.get_pcap_packet_orig_len header in
         
         let ts_sec = H.get_pcap_packet_ts_sec header in
         let ts_usec = H.get_pcap_packet_ts_usec header in

         let pcap_header =
           Pcap_header.new_t
             (Pcap_timeval.new_t
                (Int32.to_int ts_sec)
                (Int32.to_int ts_usec)
             )
             (Int32.to_int caplen)
             (Int32.to_int len)
         in

         let packet_string = Cstruct.to_string packet in

         function_to_apply
           pcap_header
           packet_string;

         ()
      )
      buf_tuple_iter
      ()
  )
