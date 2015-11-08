
open Printf

open Sexplib.Std

module Admd_ipaddr_custom = struct
  include Admd.Ipaddr_sb

  let to_float _ = 0.
  let default_value () =
    V4 (Int32.of_int 0)
      
end

module Ipaddr_distribution = Key_occurrence_distribution.Make(Admd_ipaddr_custom)
    
(* module Ipaddr_distribution = Distribution_metric.Make(Admd.Ipaddr_sb. *)
    
module Transport_protocol_for_metrics_ =
struct
  type t = Transport_protocol_for_metrics.t
  with sexp, bin_io

  let compare = Transport_protocol_for_metrics.compare
  let to_float t = float_of_int (Transport_protocol_for_metrics.to_int t)
  let to_string = Transport_protocol_for_metrics.to_string

  let default_value () = Transport_protocol_for_metrics.Other (-5)
  
  let sexp_of_t = Transport_protocol_for_metrics.sexp_of_t
  let t_of_sexp = Transport_protocol_for_metrics.t_of_sexp

  let hash = Hashtbl.hash

  (* let min = return_null_value () *)
  (* let max = return_null_value () *)

  (* let pred t = t *)
  (* let succ t = t *)

  (* let sub t1 t2 =  *)
  (*   Batteries.Int.sub *)
  (*     (Transport_protocol_for_metrics.to_int t1) *)
  (*     (Transport_protocol_for_metrics.to_int t2) *)

end
module Transport_protocol_for_metrics_distribution = Key_occurrence_distribution.Make(Transport_protocol_for_metrics_)

module Packet_fingerprint_distribution = Key_occurrence_distribution.Make(Packet_fingerprint)
