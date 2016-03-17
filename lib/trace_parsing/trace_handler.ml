
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
