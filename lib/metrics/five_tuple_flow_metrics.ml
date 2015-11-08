
open Printf

open Key_occurrence_distribution_instantiations

open Network_traffic_metric_instantiations

exception Inconsistent_statistics

let use_verification = ref false
    
type t =
  {
    mutable timestamp_sec_start : int;
    mutable timestamp_usec_start : int;
    mutable timestamp_sec_end : int;
    mutable timestamp_usec_end : int;

    mutable nb_packets : int;
    mutable nb_bytes : int;
    packet_size_distribution : Int_distribution.C.t;

    packet_fingerprint_distribution : Packet_fingerprint_distribution.C.t;

    five_tuple_flow_transport_layer_metrics : Five_tuple_flow_transport_layer_metrics.t;
  }

let new_t
    timestamp_sec_start
    timestamp_usec_start
    timestamp_sec_end
    timestamp_usec_end

    nb_packets
    nb_bytes
    packet_size_distribution

    packet_fingerprint_distribution

    five_tuple_flow_transport_layer_metrics
  =
  {
    timestamp_sec_start;
    timestamp_usec_start;
    timestamp_sec_end;
    timestamp_usec_end;

    nb_packets;
    nb_bytes;
    packet_size_distribution;

    packet_fingerprint_distribution;

    five_tuple_flow_transport_layer_metrics;
  }

let to_string
    t
  =
  let span_start = 
    Core.Span.create
      ~sec: t.timestamp_sec_start
      ~us: t.timestamp_usec_start
      ()
  in
  let time_start = Core.Time.add Core.Time.epoch span_start in
  let span_end = 
    Core.Span.create
      ~sec: t.timestamp_sec_end
      ~us: t.timestamp_usec_end
      ()
  in
  let time_end = Core.Time.add Core.Time.epoch span_end in

  sprintf
    "%d.%d (%s)\n%d.%d (%s)\nnb_packets: %d\nnb_bytes: %d\npacket_fingerprint: %s"
    t.timestamp_sec_start
    t.timestamp_usec_start
    (Core.Time.to_string_trimmed ~zone: Core.Time.Zone.local time_start)
    t.timestamp_sec_end
    t.timestamp_usec_end
    (Core.Time.to_string_trimmed ~zone: Core.Time.Zone.local time_end)
    t.nb_packets
    t.nb_bytes
    (Packet_fingerprint_distribution.C.to_string t.packet_fingerprint_distribution)

let verify error_string t =
  (
    (* If the anomaly is not empty, we check that no timestamp is
       equal to epoch *)
    if
      (
        (t.nb_packets > 0)
        &&
        ((t.timestamp_sec_start = 0 && t.timestamp_usec_start = 0) || (t.timestamp_sec_end = 0 && t.timestamp_usec_end = 0))
      ) 
    then
      (
        print_endline
          (sprintf
             "[Five_tuple_flow_metrics]: verify: timestamp equal to epoch :\n%s"
             (to_string
                t
             )
          );
        assert(false)
      );

    let number_packet_size = Int_distribution.C.get_total_nb_occurrence t.packet_size_distribution in
    if number_packet_size <> t.nb_packets then
      (
        print_endline
          (sprintf
             "[Five_tuple_flow_metrics]: verify: inconsistency between number of packet size occurence (%d) and total number of packets (%d):\n%s\n\n%s\n\n"
             number_packet_size
             t.nb_packets
             (to_string
                t
             )

             error_string
          );

        raise Inconsistent_statistics
      );

    let number_packet_fingerprint = Packet_fingerprint_distribution.C.get_total_nb_occurrence t.packet_fingerprint_distribution in
    if number_packet_fingerprint <> t.nb_packets then
      (
        print_endline
          (sprintf
             "[Five_tuple_flow_metrics]: verify: inconsistency between number of packet fingerprint occurence (%d) and total number of packets (%d):\n%s\n\n%s\n\n"
             number_packet_fingerprint
             t.nb_packets
             (to_string
                t
             )

             error_string
          );

        raise Inconsistent_statistics
      );

    Five_tuple_flow_transport_layer_metrics.verify
      t.nb_packets
      error_string
      t.five_tuple_flow_transport_layer_metrics
  )

let copy t =
  new_t
    t.timestamp_sec_start
    t.timestamp_usec_start
    t.timestamp_sec_end
    t.timestamp_usec_end

    t.nb_packets
    t.nb_bytes
    (Int_distribution.C.copy t.packet_size_distribution)

    (Packet_fingerprint_distribution.C.copy t.packet_fingerprint_distribution)

    (Five_tuple_flow_transport_layer_metrics.copy t.five_tuple_flow_transport_layer_metrics)

let append t t_to_append
    =
  (
    failwith "DO NOT USE"
  )

let fusion
    t_1
    t_2
    =
  (
    failwith "DO NOT USE"
  )

let of_packet_data_for_metrics
    packet_data_for_metrics
  =
  (
    if packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec = 0 then
      (
        assert(false)
      );

    let timestamp_sec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec in
    let timestamp_usec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_usec in
    let length = packet_data_for_metrics.Packet_data_for_metrics.length in

    let packet_size_distribution =
      Int_distribution.C.of_single_value
        length
    in

    let packet_fingerprint =
      Packet_fingerprint.of_packet_data_for_metrics
        packet_data_for_metrics
    in
    let packet_fingerprint_distribution =
      Packet_fingerprint_distribution.C.of_single_value
        packet_fingerprint
    in

    let five_tuple_flow_transport_layer_metrics =
      Five_tuple_flow_transport_layer_metrics.of_packet_data_for_metrics
        packet_data_for_metrics
    in

    let t =
      new_t
        timestamp_sec
        timestamp_usec
        timestamp_sec
        timestamp_usec 

        1
        length
        packet_size_distribution

        packet_fingerprint_distribution

        five_tuple_flow_transport_layer_metrics
    in

    if !use_verification then
      verify "of_packet_data_for_metrics" t;

    t
  )

let update
    t
    packet_data_for_metrics
  =
  (
    if packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec = 0 then
      (
        assert(false)
      );

    let packet_timestamp_sec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_sec in
    let packet_timestamp_usec = packet_data_for_metrics.Packet_data_for_metrics.timestamp_usec in

    let (timestamp_sec_start, timestamp_usec_start) =
      if t.timestamp_sec_start <> packet_timestamp_sec then
        (
          if t.timestamp_sec_start < packet_timestamp_sec then
            (t.timestamp_sec_start, t.timestamp_usec_start)
          else
            (packet_timestamp_sec, packet_timestamp_usec)
        )
      else
        (t.timestamp_sec_start
         ,
         min
           t.timestamp_usec_start
           packet_timestamp_usec
        )
    in
    t.timestamp_sec_start <- timestamp_sec_start;
    t.timestamp_usec_start <- timestamp_usec_start;

    let (timestamp_sec_end, timestamp_usec_end) =
      if t.timestamp_sec_end <> packet_timestamp_sec then
        (
          if t.timestamp_sec_end > packet_timestamp_sec then
            (t.timestamp_sec_end, t.timestamp_usec_end)
          else
            (packet_timestamp_sec, packet_timestamp_usec)
        )
      else
        (t.timestamp_sec_end
         ,
         max
           t.timestamp_usec_end
           packet_timestamp_usec
        )
    in
    t.timestamp_sec_end <- timestamp_sec_end;
    t.timestamp_usec_end <- timestamp_usec_end;

    t.nb_packets <- t.nb_packets + 1;
    t.nb_bytes <- t.nb_bytes + packet_data_for_metrics.Packet_data_for_metrics.length;
    Int_distribution.C.add_single_occurrence
      t.packet_size_distribution
      packet_data_for_metrics.Packet_data_for_metrics.length;

    let packet_fingerprint =
      Packet_fingerprint.of_packet_data_for_metrics
        packet_data_for_metrics
    in
    Packet_fingerprint_distribution.C.add_single_occurrence
      t.packet_fingerprint_distribution
      packet_fingerprint;

    Five_tuple_flow_transport_layer_metrics.update
      t.five_tuple_flow_transport_layer_metrics
      packet_data_for_metrics;

    if !use_verification then
      verify 
        (sprintf
           "update:\n%s"
           (to_string
              t
           )
        )
        t;
  )

let to_string
    t
  =
  let span_start = 
    Core.Span.create
      ~sec: t.timestamp_sec_start
      ~us: t.timestamp_usec_start
      ()
  in
  let time_start = Core.Time.add Core.Time.epoch span_start in
  let span_end = 
    Core.Span.create
      ~sec: t.timestamp_sec_end
      ~us: t.timestamp_usec_end
      ()
  in
  let time_end = Core.Time.add Core.Time.epoch span_end in

  sprintf
    "5tuple:\nStart: %d.%d (%s)\nEnd: %d.%d (%s)\nNb Packets: %d\nNb bytes: %d\npacket_fingerprint: %s\n%s"
    t.timestamp_sec_start
    t.timestamp_usec_start
    (Core.Time.to_string_trimmed ~zone: Core.Time.Zone.local time_start)
    t.timestamp_sec_end
    t.timestamp_usec_end
    (Core.Time.to_string_trimmed ~zone: Core.Time.Zone.local time_end)
    t.nb_packets
    t.nb_bytes
    (Packet_fingerprint_distribution.C.to_string t.packet_fingerprint_distribution)
    (Five_tuple_flow_transport_layer_metrics.to_string
       t.five_tuple_flow_transport_layer_metrics)
