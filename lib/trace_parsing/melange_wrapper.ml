
let env = Mpl_stdlib.new_env (String.make 1000000 '\000');;

let launch_function_on_header_ethernet
    function_to_apply
    pcap_header
    pcap_payload
  =
  Mpl_stdlib.reset env;
  Mpl_stdlib.fill_string env pcap_payload;

  if (Mpl_stdlib.size env) <= 14 then
    ()
  else
    (
      try
        (
          let ether_pdu = Ethernet.Ethernet.unmarshal env in

          function_to_apply
            pcap_header
            ether_pdu;
        )

      with
      | Ethernet.Ethernet.Bad_packet message_string -> ()
      (* | exn -> () *)
    )
