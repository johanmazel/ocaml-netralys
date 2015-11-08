
open Icmp

type t =
| Echo_request
| Echo_reply
| Destination_unreachable of int
| Other

let of_melange icmp =
  match icmp with
  | `EchoRequest echo_request -> 
    Echo_request
  | `EchoReply echo_reply ->
    Echo_request
  | `DestinationUnreachable destination_unreachable ->
    (
      let code = destination_unreachable#code in

      Destination_unreachable code
    )
  | _ -> 
    Other

