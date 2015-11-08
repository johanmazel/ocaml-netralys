
open Printf

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Trace_handler]: %s@." s)
      else
        ignore
    )
    fmt

(* let build_trace_statistics_of_trace_path *)
(*     pcap_loop *)
(*     trace_path *)
(*   = *)
(*   ( *)
(*     debug "of_trace_path: call"; *)

(*     debug "of_trace_path: scanning"; *)

(*     let ref_start_time_of_capture_acquired = ref false in *)
(*     let ref_start_time_of_capture = ref 0.0 in *)
(*     let ref_end_time_of_capture = ref 0.0 in *)

(*     let ref_total_nb_packets = ref 0 in *)
(*     let ref_size_trace = ref 0 in *)

(*     pcap_loop *)
(*       trace_path *)
(*       (fun pcap_header pcap_payload -> *)
(*          ( *)
(*            ref_total_nb_packets := !ref_total_nb_packets + 1; *)

(*            assert(pcap_header.Pcap_header.caplen > 0); *)
(*            ref_size_trace := !ref_size_trace + pcap_header.Pcap_header.caplen; *)

(*            ref_end_time_of_capture := *)
(*              Pcap_timeval.get_capture_time_of_packet_in_float *)
(*                pcap_header.Pcap_header.ts; *)

(*            if !ref_start_time_of_capture_acquired = false then *)
(*              ( *)
(*                ref_start_time_of_capture := *)
(*                  Pcap_timeval.get_capture_time_of_packet_in_float *)
(*                    pcap_header.Pcap_header.ts; *)
(*                ref_start_time_of_capture_acquired := true *)
(*              ) *)
(*          ) *)
(*       ); *)

(*     debug *)
(*       "of_trace_path: total number of packets: %d" *)
(*       !ref_total_nb_packets; *)

(*     debug *)
(*       "of_trace_path: size of packets in trace: %fMb" *)
(*       ((float_of_int !ref_size_trace) /. (1024.0 *. 1024.0)); *)

(*     debug "of_trace_path: end"; *)

(*     Trace_statistics.new_t *)
(*       trace_path *)

(*       !ref_start_time_of_capture *)
(*       !ref_end_time_of_capture *)

(*       !ref_total_nb_packets *)
(*       !ref_size_trace *)
(*   ) *)

let launch_analysis
    pcap_loop
    trace_statistics
    function_to_apply
  =
  (
    debug
      "launch_analysis: call";

    let progress_manager =
      Progress_manager.of_nb_elements_size
        trace_statistics.Trace_statistics.total_nb_packets
        trace_statistics.Trace_statistics.size_trace
    in

    debug
      "launch_analysis: start analysis";

    pcap_loop
      trace_statistics.Trace_statistics.trace_path
      (fun pcap_header pcap_payload ->
         let packet_size = pcap_header.Pcap_header.caplen in
         Progress_manager.update_and_display_stat 
           ~display_size: true
           progress_manager
           packet_size;

         function_to_apply pcap_header pcap_payload
      );

    Progress_manager.final_display
      ~display_size: true
      progress_manager;

    debug
      "launch_analysis: analysis finished";

    debug
      "launch_analysis: end";
  )
