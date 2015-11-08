
open Printf

open Sexplib.Std
open Bin_prot.Std

module IntModule =
struct
  type t = int
  with sexp, bin_io

  let compare = Batteries.Int.compare
  let to_float x = float_of_int x
  let to_string x = sprintf "%d" x

  let default_value () = 0
    
  let add = Batteries.Int.add

end

(* module IntSimple = Simple_metric(IntModule) *)
module Int_distribution = Key_occurrence_distribution.Make(IntModule)

module Int32Module = struct
  type t = int32
  with sexp, bin_io

  let compare = Int32.compare
  let to_float x = Int32.to_float x
  let to_string x =
    sprintf "%s" (Int32.to_string x)

  let default_value () = Int32.of_int 0
end

module Int32_distribution = Key_occurrence_distribution.Make(Int32Module)

module FloatModule = struct
  type t = float
  with sexp, bin_io

  let compare = Batteries.Float.compare
  let to_float (value : float) = value
  let to_string x = sprintf "%f" x
      
  let default_value () = 0.0
end

module Float_distribution = Key_occurrence_distribution.Make(FloatModule)

module StringModule = struct
  type t = string
  with sexp, bin_io

  let compare = Batteries.String.compare
  let to_float value = 0.0
  let to_string x = x

  let default_value () = ""
    
  let hash = Hashtbl.hash

  let min = "zzzzzzzzzzzzzzzzz"
  let max = "aaaaaaaaaaaaaaaaa"

  let pred t =
    let length = String.length t in
    let last_character_indice = length - 1 in
    let last_character = String.get t last_character_indice in
    (* String.set  *)
    (*   t *)
    (*   last_character_indice *)
    (*   (Char.chr (Batteries.Int.pred (Char.code t.[last_character_indice]))); *)
    (* t *)
    let new_last_character =
      Char.chr (Batteries.Int.pred (Char.code last_character))
    in
    (
      (String.sub t 0 (length - 1))    
      ^
  (sprintf "%c" new_last_character)
    )

  let succ t = 
    let length = String.length t in
    let last_character_indice = String.length t - 1 in
    let last_character = String.get t last_character_indice in
    let new_last_character =
      Char.chr (Batteries.Int.succ (Char.code last_character))
    in
    (
      (String.sub t 0 (length - 1))    
      ^
  (sprintf "%c" new_last_character)
    )

  let sub t1 t2 = 0

end

module String_distribution = Key_occurrence_distribution.Make(StringModule)

