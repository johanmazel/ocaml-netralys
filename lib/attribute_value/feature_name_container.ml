
open Printf

open Sexplib.Std
open Bin_prot.Std

let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
  (fun s -> Format.printf "[Feature_name_container]: %s@." s)
      else
  ignore
    )
    fmt

type t =
  {
    name_int_map : string Core.Std.Int.Map.t;
    indice_string_map : int Core.Std.String.Map.t;
  }
[@@deriving compare, sexp, bin_io]

let new_t
    name_int_map
    indice_string_map
  =
  {
    name_int_map;
    indice_string_map;
  }

let to_string_csv t =
  (* Core_maps.to_string_int_map *)
  (*   ~sep_element: "," *)
  (*   ~sep_key_value: "" *)
  (*   (fun key -> "") *)
  (*   (fun attribute_name -> attribute_name) *)
  (*   t.name_int_map *)
  let l = Core.Std.Int.Map.to_alist t.name_int_map in

  List_ext.to_string
    ~sep: ","
    (fun (_, attribute_name) -> attribute_name)
    l

let find_name t indice = 
  (* try *)
  Core.Std.Int.Map.find_exn t.name_int_map indice
  (* with *)
  (* | Not_found -> failwith (sprintf "[Feature_name_container}: could not find attribute name for indice %d" indice) *)

let find_indice t name = 
  Core.Std.String.Map.find_exn t.indice_string_map name
  (* with *)
  (* | Not_found -> failwith (sprintf "[Feature_name_container]: could not find attribute indice for name %s" name) *)

let of_indice_name_tuple_array
    indice_attribute_name_tuple_array
  =
  let int_map =
    Array.fold_left
      (fun int_map (indice, attribute_name) ->
         Core.Std.Int.Map.add
           int_map
           indice
           attribute_name
      )
      Core.Std.Int.Map.empty
      indice_attribute_name_tuple_array
  in

  let string_map =
    Array.fold_left
      (fun string_map (indice, attribute_name) ->
         Core.Std.String.Map.add
           string_map
           attribute_name
           indice
      )
      Core.Std.String.Map.empty
      indice_attribute_name_tuple_array
  in

  new_t
    int_map
    string_map

let to_list_attribute_name t =
  Core.Std.Int.Map.data t.name_int_map
