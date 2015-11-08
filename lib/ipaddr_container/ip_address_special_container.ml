
module L = BatList
  
open Ip_address_data_structures_instantiations

type t =
  | V4 of Ip_address_data_structures_V4.Container_special.t
  | V6 of Ip_address_data_structures_V6.Container_special.t
(* with compare *)
with compare, sexp, bin_io
  
(* let length t = *)
(*   match t with *)
(*   | V4 container -> Ip_address_data_structures_V4.Container_special.length container *)

let new_empty_v4 () =
  V4 (Ip_address_data_structures_V4.Container_special.new_empty_t ())

let of_container_compact_list container_compact_list =
  (* assert(L.length container_compact_list > 0); *)

  (* let l = *)
  (*   L.map *)
  (*     (fun container_compact -> *)
  (*        match container_compact with *)
  (*        | Ip_address_container_compact.Empty -> raise (Invalid_argument "[Ip_address_container_special]: of_container_compact_list: Empty when expected IPv4") *)
  (*        | Ip_address_container_compact.V4 container_compact -> container_compact *)
  (*        | Ip_address_container_compact.V6 _ -> raise (Invalid_argument "[Ip_address_container_special]: of_container_compact_list: IPv6 when expected IPv4") *)
  (*     ) *)
  (*     container_compact_list *)
  (* in *)

  (* let new_t : t = *)
  (*   V4 *)
  (*     (Ip_address_data_structures_V4.Container_special.of_container_compact_list *)
  (*        l *)
  (*     ) *)
  (* in *)

  (* new_t *)

  assert(L.length container_compact_list > 0);

  let first_container = L.hd container_compact_list in

  match first_container with
  | Ip_address_container_compact.Empty ->
    raise (Invalid_argument "[Ip_address_container_special]: of_container_compact_list: Empty")
  | Ip_address_container_compact.V4 container ->
    let l =
      L.map
        (fun container_compact ->
           match container_compact with
           | Ip_address_container_compact.Empty -> raise (Invalid_argument "[Ip_address_container_special]: of_container_compact_list: Empty when expected IPv4")
           | Ip_address_container_compact.V4 container_compact -> container_compact
           | Ip_address_container_compact.V6 _ -> raise (Invalid_argument "[Ip_address_container_special]: of_container_compact_list: IPv6 when expected IPv4")
        )
        container_compact_list
    in

    V4
      (Ip_address_data_structures_V4.Container_special.of_container_compact_list
         l
      )
  | Ip_address_container_compact.V6 container ->
    let l =
      L.map
        (fun container_compact ->
           match container_compact with
           | Ip_address_container_compact.Empty -> raise (Invalid_argument "[Ip_address_container_special]: of_container_compact_list: Empty when expected IPv6")
           | Ip_address_container_compact.V4 _ -> raise (Invalid_argument "[Ip_address_container_special]: of_container_compact_list: IPv4 when expected IPv6")
           | Ip_address_container_compact.V6 container_compact -> container_compact
        )
        container_compact_list
    in
    
    V6
      (Ip_address_data_structures_V6.Container_special.of_container_compact_list
         l
      )

(* let fold f t init = *)
(*   match t with *)
(*   | V4 container -> Ip_address_data_structures_V4.Container_special.fold f container init *)
    
let fold_container f t init =
  match t with
  | V4 container ->
    Ip_address_data_structures_V4.Container_special.fold_container
      (fun indice (container : Ip_address_data_structures_V4.Container.t) acc ->
         let ip_address_container =
           Ip_address_container.V4
             container
         in

         f
           indice
           ip_address_container
           acc
      )
      container
      init
  | V6 container ->
    Ip_address_data_structures_V6.Container_special.fold_container
      (fun indice container acc ->
         let ip_address_container =
           Ip_address_container.V6
             container
         in

         f
           indice
           ip_address_container
           acc
      )
      container
      init
    
let copy t =
  match t with
  | V4 container ->
    V4
      (Ip_address_data_structures_V4.Container_special.copy
         container
      )
  | V6 container ->
    V6
      (Ip_address_data_structures_V6.Container_special.copy
         container
      )

let find_element_indice t indice =
  match t with
  | V4 container ->
    let bin_int_set =
      Ip_address_data_structures_V4.Container_special.find_element_indice
        container
        indice
    in

    let unsigned_int_set =
      Ip_address_data_structures_V4.Unsigned_int_set.of_bin_int_set
        bin_int_set
    in

    Ip_address_container.V4
      (Ip_address_data_structures_V4.Container.new_t
         unsigned_int_set
      )
  | V6 container ->
    let bin_int_set =
      Ip_address_data_structures_V6.Container_special.find_element_indice
        container
        indice
    in

    let unsigned_int_set =
      Ip_address_data_structures_V6.Unsigned_int_set.of_bin_int_set
        bin_int_set
    in

    Ip_address_container.V6
      (Ip_address_data_structures_V6.Container.new_t
         unsigned_int_set
      )
      
(* let inter t1 t2 = *)
(*   match t1,t2 with *)
(*   | V4 a, V4 b -> V4 (Ip_address_data_structures_V4.Container_special.inter a b) *)

let remove_element_indice t indice =
  match t with
  | V4 container ->
    Ip_address_data_structures_V4.Container_special.remove_element_indice
      container
      indice
  | V6 container ->
    Ip_address_data_structures_V6.Container_special.remove_element_indice
      container
      indice

(* let add t indice src_int32_set = *)
(*   match t with *)
(*   | V4 container -> *)
(*     Ip_address_data_structures_V4.Container_special.add *)
(*       container *)
(*       indice *)
(*       src_int32_set *)
(*   | V6 container -> *)
(*     Ip_address_data_structures_V6.Container_special.add *)
(*       container *)
(*       indice *)
(*       src_int32_set *)

let relative_inter_length t indice =
  match t with
  | V4 container ->
    Ip_address_data_structures_V4.Container_special.relative_inter_length
      container
      indice
  | V6 container ->
    Ip_address_data_structures_V6.Container_special.relative_inter_length
      container
      indice

let inter_length_global t =
  match t with
  | V4 container ->
    Ip_address_data_structures_V4.Container_special.inter_length_global
      container
  | V6 container ->
    Ip_address_data_structures_V6.Container_special.inter_length_global
      container      

let union_length_global t =
  match t with
  | V4 container ->
    Ip_address_data_structures_V4.Container_special.union_length_global
      container
  | V6 container ->
    Ip_address_data_structures_V6.Container_special.union_length_global
      container
      
