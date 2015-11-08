
open Printf

module A = BatArray
module L = BatList

open Sexplib.Std
open Bin_prot.Std

module type KEY = sig
  type t
  val compare : t -> t -> int
    
  val to_float : t -> float
  val to_string : t -> string
    
  val default_value : unit -> t

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  val bin_t : t Bin_prot.Type_class.t
  val bin_size_t : t -> int
  val bin_write_t : Bin_prot.Common.buf -> pos:Bin_prot.Common.pos -> t -> Bin_prot.Common.pos
  val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
  val bin_writer_t : t Bin_prot.Type_class.writer
  val bin_reader_t : t Bin_prot.Type_class.reader
  val __bin_read_t__ : (int -> t) Bin_prot.Read.reader
end

module Make = functor (Key : KEY) -> struct

  module Key_set = Batteries.Set.Make(Key);;

  module Key_core_map = struct
    include Core.Core_map.Make(
      struct
        type t = Key.t
        with sexp, bin_io

        let compare = Key.compare
        let to_string = Key.to_string
      end
      )

    let to_string
        ?first: (first = "")
        ?last: (last = "")
        ?sep: (sep = "")
        to_string_key_value
        map
      =
      let list = to_alist map in

      List_ext.to_string
        ~first: first
        ~last: last
        ~sep: sep
        (fun (key, value) -> to_string_key_value key value)
        list

  end

  module Container = struct
    
    type 'a t =
      {
        hashtable : (Key.t, 'a) Core.Std.Hashtbl.Poly.t;
      }
    with sexp, bin_io

    let new_t hashtable = { hashtable }

    let create size = new_t (Core.Std.Hashtbl.Poly.create ())
    let length t = Core.Std.Hashtbl.Poly.length t.hashtable

    let find t key =
      Core.Std.Hashtbl.Poly.find_exn t.hashtable key
        
    let add t key value = Core.Std.Hashtbl.Poly.add_exn t.hashtable key value
    let replace t key value = Core.Std.Hashtbl.Poly.replace t.hashtable key value
    let modify t key f =
      try(
        let nb_occurence_found = find t key in

        replace
          t
          key
          (f nb_occurence_found)
      )
      with
      | Not_found ->
        (
          let _result =
            add
              t
              key
              (f (Some 0))
          in

          ()
        )

    let copy t = new_t (Core.Std.Hashtbl.Poly.copy t.hashtable)

    let keys t = Core.Std.Hashtbl.Poly.keys t.hashtable
    let occurrences t = Core.Std.Hashtbl.Poly.data t.hashtable

    let bindings t =
      Core.Std.Hashtbl.Poly.to_alist t.hashtable
      
    let bindings_ordered t =
      let list = Core.Std.Hashtbl.Poly.to_alist t.hashtable in

      let key_core_map =
        Key_core_map.of_alist_exn
          list
      in

      Key_core_map.to_alist key_core_map
      
    let of_list l =
      let result =
        Core.Std.Hashtbl.Poly.of_alist l
      in
      match result with
      | `Duplicate_key key ->
        raise
          (Invalid_argument
             (sprintf
                "[Dictribution_metric]: of_list: duplicate key: %s"
                (Key.to_string key)
             )
          )
      | `Ok h ->
        new_t
          h

    let compare
        f
        t1
        t2
      =
      Hashtbl_utils_instantiations.Core_hashtbl_utils.compare
        f
        t1.hashtable
        t2.hashtable

    let iter f t = Core.Std.Hashtbl.Poly.iter t.hashtable (fun ~key: key ~data: data -> f key data)

    let fold f t acc =
      let result : 'c =
        Core.Std.Hashtbl.Poly.fold
          t.hashtable
          ~f: (fun ~key: key ~data: data (acc : 'c) -> f key data acc)
          ~init: acc
      in

      result

    let filteri f t =
      new_t
        (Core.Std.Hashtbl.Poly.filteri t.hashtable (fun ~key: key ~data: data -> f key data))

    let to_string
        ?sep: (sep = "")
        ?sep_key_value: (sep_key_value = " ")
        ?to_string_key: (to_string_key = (fun _ -> ""))
        to_string_value
        t
      =
      let key_map =
        Key_core_map.of_alist_exn
          (bindings t)
      in

      Key_core_map.to_string
        ~sep: sep
        (fun key value ->
           sprintf
             "%s%s%s"
             (Key.to_string key)
             sep_key_value
             (to_string_value value)
        )
        key_map

  end

  module Container_float = struct
    
    type t = float Container.t

    let add_occurrence
        t 
        key
        occurrence
      =
      try
        (* If it is not in set but in hashtable, we modify
           hashtable. *)
        let nb_occurrence_found = 
          Container.find t key 
        in

        Container.replace
          t
          key
          (nb_occurrence_found +. occurrence)
      with
      | Not_found ->
        (
          (* If it is not in set nor in hashtable, we add to
             hashtable. *)
          Container.add
            t
            key
            occurrence;
        )

    let copy t =
      (Container.copy t)

    let append t t_to_append =
      (
        Container.iter
          (fun key nb_occurrence ->
             (
               (* assert(nb_occurrence >= 2); *)

               add_occurrence
                 t
                 key
                 nb_occurrence
             )
          )
          t_to_append;
      )

    let bindings t =
      Container.bindings t

    let to_string
        t
      =
      let length = Container.length t in
      (sprintf "Nb different key values: %d" 
         (Container.length t)
      )
      ^
      (if length < 40 then
         ":\n"
         ^
         (Container.to_string
            ~sep: ";"
            ~sep_key_value: " "
            ~to_string_key: Key.to_string
            (fun nb_occurence -> string_of_float nb_occurence)
            t
         )
       else
         ""
      )
      (* match to_string_mode with *)
      (* | To_string_mode.Command -> *)
      (*   sprintf "Nb different key values: %d" (Container.length t) *)
      (* | To_string_mode.Simple -> *)
      (*   let length = Container.length t in *)
      (*   (sprintf "Nb different key values: %d"  *)
      (*      (Container.length t) *)
      (*   ) *)
      (*   ^ *)
      (*   (if length < 40 then *)
      (*      ":\n" *)
      (*      ^ *)
      (*      (Container.to_string *)
      (*         ~sep: ";" *)
      (*         ~sep_key_value: " " *)
      (*         ~to_string_key: Key.to_string *)
      (*         (fun nb_occurence -> string_of_float nb_occurence) *)
      (*         t *)
      (*      ) *)
      (*    else *)
      (*      "" *)
      (*   ) *)
      (* | To_string_mode.Normal -> *)
      (*   (sprintf "Nb different key values: %d"  *)
      (*      (Container.length t) *)
      (*   ) *)
      (*   ^ *)
      (*   ":\n" *)
      (*   ^ *)
      (*   (Container.to_string *)
      (*      ~sep: ";" *)
      (*      ~sep_key_value: " " *)
      (*      ~to_string_key: Key.to_string *)
      (*      (fun nb_occurence -> string_of_float nb_occurence) *)
      (*      t *)
      (*   ) *)

  end

  module C = struct
  
    type t =
      {
        value_container_hashtable : int Container.t;
      }
    with sexp, bin_io

    let new_t
        value_container_hashtable
      =
      {
        value_container_hashtable;
      }

    let new_empty_t
        ()
      =
      new_t
        (Container.create 0)
      
    let new_single_t
        ()
      =
      new_t
        (Container.create 1)
      
    let length
        t
      =
      Container.length t.value_container_hashtable
      
    let get_total_nb_occurrence
        t
      =
      Container.fold
        (fun type_for_distribution occurence acc -> acc + occurence)
        t.value_container_hashtable
        0
        
    (* let to_string *)
    (*     to_string_mode *)
    (*     t *)
    (*   = *)
    (*   match to_string_mode with *)
    (*   | To_string_mode.Command -> *)
    (*     sprintf "Nb different values: %d" (length t) *)
    (*   | To_string_mode.Simple -> *)
    (*     let length = length t in *)
    (*     (sprintf "Nb different key values: %d"  *)
    (*        length *)
    (*     ) *)
    (*     ^ *)
    (*     (if length < 40 then *)
    (*        "\n: " *)
    (*        ^ *)
    (*        (Container.to_string *)
    (*           ~sep: ";" *)
    (*           ~sep_key_value: " " *)
    (*           ~to_string_key: Key.to_string *)
    (*           (fun nb_occurence -> string_of_int nb_occurence) *)
    (*           t.value_container_hashtable *)
    (*        ) *)
    (*      else *)
    (*        "" *)
    (*     ) *)
    (*   | To_string_mode.Normal -> *)
    (*     (sprintf "Nb different key values: %d:\n%s"  *)
    (*        (length t) *)
    (*        (Container.to_string *)
    (*           ~sep: ";" *)
    (*           ~sep_key_value: " " *)
    (*           ~to_string_key: Key.to_string *)
    (*           (fun nb_occurence -> string_of_int nb_occurence) *)
    (*           t.value_container_hashtable *)
    (*        ) *)
    (*     ) *)
      
    let to_string_full
        t
      =
      sprintf
        "Nb different key values: %d:\n%s"
        (length t)
        (Container.to_string
           ~sep: ";"
           ~sep_key_value: " "
           ~to_string_key: Key.to_string
           (fun nb_occurence -> string_of_int nb_occurence)
           t.value_container_hashtable
        )
      
          
    let to_string
        t
      =
      let length = length t in
      (sprintf "Nb different key values: %d (%d)" 
         length
         (get_total_nb_occurrence t)
      )
      ^
      (if length < 40 then
         ":\n"
         ^
         (Container.to_string
            ~sep: ";"
            ~sep_key_value: " "
            ~to_string_key: Key.to_string
            (fun nb_occurence -> string_of_int nb_occurence)
            t.value_container_hashtable
         )
       else
         ""
      )
      (* match to_string_mode with *)
      (* | To_string_mode.Command -> *)
      (*   sprintf "Nb different key values: %d" (length t) *)
      (* | To_string_mode.Simple -> *)
      (*   let length = length t in *)
      (*   (sprintf "Nb different key values: %d (%d)"  *)
      (*      length *)
      (*      (get_total_nb_occurrence t) *)
      (*   ) *)
      (*   ^ *)
      (*   (if length < 40 then *)
      (*      ":\n" *)
      (*      ^ *)
      (*      (Container.to_string *)
      (*         ~sep: ";" *)
      (*         ~sep_key_value: " " *)
      (*         ~to_string_key: Key.to_string *)
      (*         (fun nb_occurence -> string_of_int nb_occurence) *)
      (*         t.value_container_hashtable *)
      (*      ) *)
      (*    else *)
      (*      "" *)
      (*   ) *)
      (* | To_string_mode.Normal -> *)
      (*   (sprintf "Nb different key values: %d (%d)"  *)
      (*      (length t) *)
      (*      (get_total_nb_occurrence t) *)
      (*   ) *)
      (*   ^ *)
      (*   ":\n" *)
      (*   ^ *)
    (* (Container.to_string *)
      (*      ~sep: ";" *)
      (*      ~sep_key_value: " " *)
      (*      ~to_string_key: Key.to_string *)
      (*      (fun nb_occurence -> string_of_int nb_occurence) *)
      (*      t.value_container_hashtable *)
    (* ) *)
          
    let empty t =
      if length t = 0 then
        true
      else
        false
  
    let filter f t =
      new_t
        (Container.filteri
           f
           t.value_container_hashtable
        )
                    
    let keys t =
      Container.keys t.value_container_hashtable
      
    let bindings t =
      Container.bindings t.value_container_hashtable
          
    let bindings_ordered t =
      Container.bindings_ordered t.value_container_hashtable
          
    (* TODO: add consistency check *)
    let of_float_t float_t =
      (
        let l = Container.bindings float_t in
        let key_int_tuple_l =
          L.map
            (fun (key, occurrence) -> (key, int_of_float occurrence))
            l
        in

        let t =
          Container.of_list
            key_int_tuple_l
        in

        t
      )
    
    let to_float_t t =
      (
        let l = bindings t in
        let key_float_tuple_l =
          L.map
            (fun (key, occurrence) -> (key, float_of_int occurrence))
            l
        in

        let float_t : float Container.t =
          Container.of_list
            key_float_tuple_l
        in

        float_t
      )

    let of_single_value
        key
      =
      let hashtable = Container.create 1 in
      Container.add
        hashtable
        key
        1;

      new_t
        hashtable

    let of_name_value_occurence
        name
        value
        occurrence
      =
      let hashtable = Container.create 0 in
      Container.add
        hashtable
        value
        occurrence;

      new_t
        hashtable
        
    let add_single_occurrence
        t 
        key
      =
      try
        (
          let nb_occurrence_found = 
            Container.find t.value_container_hashtable key 
          in

          Container.replace
            t.value_container_hashtable
            key
            (nb_occurrence_found + 1)
        )
      with
      | Not_found ->
        (
          Container.add
            t.value_container_hashtable
            key
            1
        )

    let add_occurrences
        t 
        key
        occurrences
      =
      try
        (
          let nb_occurrence_found = 
            Container.find t.value_container_hashtable key 
          in

          Container.replace
            t.value_container_hashtable
            key
            (nb_occurrence_found + occurrences)
        )
      with
      | Not_found ->
        (
          Container.add
            t.value_container_hashtable
            key
            occurrences
        )

    let of_list
        key_occurrence_list
      =
      let t = new_empty_t () in

      List.iter
        (fun (key, occurrence) ->
           if occurrence = 1 then
             add_single_occurrence
               t 
               key
           else
             add_occurrences
               t 
               key
               occurrence
        )
        key_occurrence_list;

      t

    let diff t1 t2 =
      (
        let bindings_1 = bindings t1 in
        let bindings_2 = bindings t2 in

        let map_1 =
          Key_core_map.of_alist_exn
            bindings_1
        in
        let map_2 =
          Key_core_map.of_alist_exn
            bindings_2
        in

        let diff_sequence =
          Key_core_map.symmetric_diff
            map_1
            map_2
            ~data_equal: Batteries.Int.equal
        in

        let diff_list =
          Core_kernel.Sequence.to_list
            diff_sequence
        in

        let key_occurrence_list =
          L.map
            (fun (key, value) ->
               match value with
               | `Left occurrence -> key, occurrence
               | `Right occurrence -> key, - occurrence
               | `Unequal (occurrence_1, occurrence_2) -> key, (occurrence_1 - occurrence_2)
            )
            diff_list
        in

        of_list
          key_occurrence_list
      )

    let copy t =
      new_t
        (Container.copy t.value_container_hashtable)
      
    let append t t_to_append =
      (
        Container.iter
          (fun key nb_occurrence ->
             add_occurrences
               t
               key
               nb_occurrence
          )
          t_to_append.value_container_hashtable;
      )
      
    let fusion t1 t2 =
      (
        let new_t =
          copy
            t1
        in

        append new_t t2;

        new_t
      )
      
    let get_one_key t =
      assert(length t > 0);
      fst
        (L.hd
           (bindings
              t
           )
        )
    
    let get_key_occurrence key t =
      try
        Container.find t.value_container_hashtable key
      with
      | Not_found ->
        0
          
    let get_smallest_occurrence_tuple t =
      (
        if length t = 0 then
          None
        else
          let ref_value_inited = ref false in

          let result =
            Container.fold
              (fun key occurrence (min_key, min_occurrence) ->
                 if !ref_value_inited = false then
                   (
                     ref_value_inited := true;
                     (key, occurrence)
                   )
                 else
                 if occurrence < min_occurrence then
                   (key, occurrence)
                 else
                   (min_key, min_occurrence)
              )
              t.value_container_hashtable
              (get_one_key t, 0)
          in

          Some result
      )
      
    let get_value_with_smallest_occurence_exn t =
      match get_smallest_occurrence_tuple t with
      | None -> raise Not_found
      | Some tuple -> tuple

    let get_biggest_occurrence_tuple t =
      if length t = 0 then
        None
      else
        let ref_value_inited = ref false in

        let result =
          Container.fold
            (fun key occurrence (min_key, min_occurrence) ->
               if !ref_value_inited = false then
                 (
                   ref_value_inited := true;
                   (key, occurrence)
                 )
               else
               if occurrence > min_occurrence then
                 (key, occurrence)
               else
                 (min_key, min_occurrence)
            )
            t.value_container_hashtable
            (get_one_key t, 0)
        in

        Some result

    let get_biggest_occurence_tuple_exn t =
      match get_biggest_occurrence_tuple t with
      | None -> raise Not_found
      | Some tuple -> tuple
    
    let get_biggest_occurrence t =
      match get_biggest_occurrence_tuple t with
      | None -> 0
      | Some (_, nb_occurrence) -> nb_occurrence

    let get_ratio_bigst_occur_over_nb_occur t =
      if (length t) = 0 then
        0.0
      else
        (float_of_int (get_biggest_occurrence t))
        /.
        (float_of_int (get_total_nb_occurrence t))
    
    let get_smallest_occurence t =
      match get_smallest_occurrence_tuple t with
      | None -> 0
      | Some (_, nb_occurrence) -> nb_occurrence

    let get_value_with_smallest_occurence t =
      match get_smallest_occurrence_tuple t with
      | None -> Key.default_value ()
      | Some (value, _) -> value
      
    let get_biggest_value t =
      match get_biggest_occurrence_tuple t with
      | None -> Key.default_value ()
      | Some (value, _) -> value
      

    

    let get_mean_key t =
      (
        let ref_sum = ref 0.0 in

        let add_value_to_ref ref_value type_for_distribution occurrence =
          (
            let value_to_add = Key.to_float type_for_distribution *. (float_of_int occurrence) in
            ref_sum := !ref_sum +. value_to_add;
          )
        in

        Container.iter
          (fun key occurrence ->
             add_value_to_ref
               ref_sum
               key
               occurrence
          )
          t.value_container_hashtable;

        let mean = !ref_sum /. (float_of_int (length t)) in
        mean;
      )

    let array1_to_array
        array1
      =
      let array_init =
        Array.make
          (Bigarray.Array1.dim array1)
          0
      in

      let array =
        Array.mapi
          (fun indice _ ->
             let value = Bigarray.Array1.get array1 indice in
             value
          )
          array_init
      in

      array
        
    let get_mean_med_std_idr_mad_key t =
      (
        let array =
          (* A.map *)
          (*   (fun key occurrence -> *)
          (*      A.make occurrence (Key.to_float key) *)
          (*   ) *)
          (*   (A.of_list *)
          (*      (Container.bindings *)
          (*         t *)
          (*      ) *)
          (*   ) *)
          Container.fold
            (fun key occurrence array_acc ->
               let a = A.make occurrence (Key.to_float key) in

               A.append array_acc a
            )
            t.value_container_hashtable
            [||]
        in

        let array1 =
          Bigarray.Array1.of_array
            Bigarray.float64
            Bigarray.c_layout
            array
        in
        Gsl.Gsl_sort.vector
          array1;

        let array_sorted =
          array1_to_array
            array1
        in

        let mean = Gsl.Stats.mean array_sorted in
        let med =
          Gsl.Stats.quantile_from_sorted_data
            array_sorted
            0.5
        in
        let std =
          if A.length array_sorted <= 1 then
            0.
          else
            Gsl.Stats.sd
              array_sorted
        in

        (* print_endline *)
        (*   (sprintf *)
        (*      "[Distribution_metric]: t: %s:\n%f %f %f" *)
        (*      (to_string To_string_mode.Normal t) *)
        (*      mean *)
        (*      med *)
        (*      std *)
        (*   ); *)

        let _10centile =
          Gsl.Stats.quantile_from_sorted_data
            array_sorted
            0.1
        in

        let _90centile =
          Gsl.Stats.quantile_from_sorted_data
            array_sorted
            0.9
        in

        let interdecile_range = abs_float (_90centile -. _10centile) in
        (* if interdecile_range < 0. then *)
        (*   ( *)
        (*     print_endline *)
        (*       (sprintf *)
        (*          "Distribution_metric: get_mean_med_std_idr_mad_key: interdecile_range < %f (%f - %f):\n%s" *)
        (*          interdecile_range *)
        (*          _90centile *)
        (*          _10centile *)
        (*          (to_string *)
        (*             To_string_mode.Normal *)
        (*             t *)
        (*          ) *)
        (*       ); *)
        (*     assert(false) *)
        (*   ); *)

        let ad_array =
          A.map
            (fun value ->
               abs_float (value -. med)
            )
            array_sorted
        in
                
        let ad_array1 =
          Bigarray.Array1.of_array
            Bigarray.float64
            Bigarray.c_layout
            ad_array
        in
        Gsl.Gsl_sort.vector
          ad_array1;

        let ad_array_sorted =
          array1_to_array
            ad_array1
        in
        
        let mad =
          Gsl.Stats.quantile_from_sorted_data
            ad_array_sorted
            0.5
        in

        (* if std > 1. && mad = 0. then *)
        (*   ( *)
        (*     print_endline *)
        (*       (sprintf *)
        (*          "Distribution_metric: get_mean_med_std_idr_mad_key: med: %f ; high std (%f) but low mad (%f):\nad_array_sorted:\n%s\n\n%s" *)
        (*          med *)
        (*          std *)
        (*          mad *)
        (*          (Utils_batteries.to_string_array *)
        (*             string_of_float *)
        (*             ad_array_sorted *)
        (*          ) *)
        (*          (to_string *)
        (*             To_string_mode.Normal *)
        (*             t *)
        (*          ) *)
        (*       ); *)
        (*     assert(false) *)
        (*   ); *)

        (* let _95centile = *)
        (*   Gsl.Stats.quantile_from_sorted_data *)
        (*     array_sorted *)
        (*     0.95 *)
        (* in *)
        (* let _95centile_int = int_of_float _95centile in *)

        (* let _99centile = *)
        (*   Gsl.Stats.quantile_from_sorted_data *)
        (*     array_sorted *)
        (*     0.99 *)
        (* in *)
        (* let _99centile_int = int_of_float _99centile in *)

        (* let _max = *)
        (*   Gsl.Stats.max *)
        (*     array_sorted *)
        (* in *)
        (* let _max_int = int_of_float _max in *)

        (* (mean, med_int, _90centile_int, _95centile_int, _99centile_int, _max_int) *)

        (mean, med, std, interdecile_range, mad)
      )

    let get_avg_nb_occurence t =
      (
        if length t = 0 then
          (
            0.0
          )
        else
          (
            let ref_sum = ref 0 in

            let add_value_to_ref ref_value nb_occurence =
              ref_sum := !ref_sum + nb_occurence;
            in

            (* Key_set.iter *)
            (*   (fun key -> *)
            (*    add_value_to_ref *)
            (*      ref_sum *)
            (*      1 *)
            (*   ) *)
            (*   t.value_with_single_occurrence;  *)

            Container.iter
              (fun _ nb_occurence -> ( add_value_to_ref ref_sum nb_occurence))
              t.value_container_hashtable;

            let mean = (float_of_int !ref_sum) /. (float_of_int (length t)) in
            mean
          )
      )

    let get_variance t =
      (
        (* If t is empty, returning 0 *)
        if length t == 0 then
          (
            0.0
          )
        else
          (
            let ref_sum = ref 0.0 in
            let ref_square_sum = ref 0.0 in

            let add_value_to_ref ref_value type_for_distribution =
              (
                let value_to_add = Key.to_float type_for_distribution in
                ref_sum := !ref_sum +. value_to_add;
                ref_square_sum := !ref_square_sum +. (value_to_add ** 2.0);
              )
            in

            (* Key_set.iter *)
            (*   (fun key -> *)
            (*    add_value_to_ref *)
            (*      ref_sum *)
            (*      key *)
            (*   ) *)
            (*   t.value_with_single_occurrence;  *)

            Container.iter
              (fun type_for_distribution nb_occurence -> 
                 add_value_to_ref
                   ref_sum
                   type_for_distribution
              )
              t.value_container_hashtable;

            let mean = !ref_sum /. (float_of_int (length t) ) in
            let mean_square = !ref_square_sum /. (float_of_int (length t) ) in
            let variance = mean_square -. (mean ** 2.0) in
            variance
          )
      )

    let jaccard t1 t2 =
      let set1 =
        List.fold_left
          (fun set (key, element) -> 
             Key_set.add
               key
               set
          )
          Key_set.empty
          (bindings t1)
      in

      let set2 =
        List.fold_left
          (fun set (key, element) -> 
             Key_set.add
               key
               set
          )
          Key_set.empty
          (bindings t2)
      in

      let inter = Key_set.inter set1 set2 in

      if Key_set.cardinal inter = 0 then
        0.
      else
        let union = Key_set.inter set1 set2 in

        if Key_set.cardinal union = 0 then
          0.
        else
          (float_of_int (Key_set.cardinal inter))
          /.
          (float_of_int (Key_set.cardinal union))

  let simpson t =
    let total_nb_occurrence_float = float_of_int (get_total_nb_occurrence t) in
    (* let element_in_set_probability = 1. /. total_nb_occurrence_float in *)

    (* let simpson_after_set = *)
    (*   (element_in_set_probability ** 2.) *. (float_of_int (Key_set.cardinal t.value_with_single_occurrence)) *)
    (* in *)

    let simpson_after_hashtable =
      Container.fold
        (fun key occurrence simpson_acc ->
           let p = float_of_int occurrence /. total_nb_occurrence_float in
           simpson_acc +. (p ** 2.)
        )
        t.value_container_hashtable
        0.
    in

    simpson_after_hashtable

  let compare
      t1
      t2
    =
    Container.compare
      Batteries.Int.compare
      t1.value_container_hashtable
      t2.value_container_hashtable
    
  let is_valid t =
    (
      Container.fold
        (fun key occurrence acc -> 
           (occurrence > 0) && acc
        )
        t.value_container_hashtable
        true
    )

  let log_2 = log 2.0
  let log2 n = log n /. log_2

  let logb b n = log n /. log b

  let shannon_entropy_of_p_list l =
    let n = L.length l in
    let histo = A.of_list l in
    if n = 0 then
      0.0
    else
      let rec loop sum ix =
        if ix < 0 then sum
        else
          let freq = histo.(ix) in
          if freq = 0. then loop sum (ix - 1)
          else
            let ffreq = freq in
            loop (sum +. ffreq *. log ffreq) (ix - 1) in
      let sum = loop 0.0 (Array.length histo - 1) in
      let f_n = float n in
      sum /. f_n

  (* http://www.gnu-darwin.org/www001/src/ports/devel/aifad/work/aifad-1.0.27/src/entropy_utils.ml *)
  let shannon_entropy t =
    let n = get_total_nb_occurrence t in
    let histo = A.of_list (L.map snd (bindings t)) in
    if n = 0 then
      0.0
    else
      let rec loop sum ix =
        if ix < 0 then sum
        else
          let freq = histo.(ix) in
          if freq = 0 then loop sum (ix - 1)
          else
            let ffreq = float freq in
            loop (sum +. ffreq *. log ffreq) (ix - 1) in
      let sum = loop 0.0 (Array.length histo - 1) in
      let f_n = float n in
      (* ((log2 f_n -. sum) /. f_n) /. log_2 *)
      log2 f_n -. ((sum /. f_n) /. log_2)

  let shannon_entropy_normalized t =
    let keys = keys t in
    let keys_length = L.length keys in
    assert(keys_length > 0);
    let value =
      if keys_length = 1 then
        0.
      else
        (shannon_entropy t)
        /.
        (log2 (float_of_int (keys_length)))
    in

    value

  let shannon_entropy_float_list l =
    let n =
      L.fold_left
        (fun acc (_, occurrence) -> acc +. occurrence)
        0.
        l
    in
    let histo = A.of_list (L.map snd l) in
    if n = 0. then
      0.0
    else
      let rec loop sum ix =
        if ix < 0 then sum
        else
          let freq = histo.(ix) in
          if freq = 0. then loop sum (ix - 1)
          else
            (* let ffreq = freq in *)
            loop (sum +. freq *. log freq) (ix - 1) in
      let sum = loop 0.0 (Array.length histo - 1) in
      (* let f_n = float n in *)
      (* ((log2 f_n -. sum) /. f_n) /. log_2 *)
      log2 n -. ((sum /. n) /. log_2)

  let shannon_entropy_normalized_float_list l =
    let length = L.length l in
    assert(length > 0);
    let value =
      if length = 1 then
        0.
      else
        (shannon_entropy_float_list l)
        /.
        (log2 (float_of_int (length)))
    in

    (* print_endline *)
    (*   (sprintf *)
    (*          "Distribution_metric: shannon_entropy_normalized: %f:\n%s" *)
    (*          value *)
    (*          (to_string *)
    (*             To_string_mode.Normal *)
    (*             t *)
    (*          ) *)
    (*   ); *)

    (* assert(value >= 0.); *)
    (* assert(value <= 1.); *)

    value

  let kullback_leibler_old t1 t2 =
    (
      (* print_endline "Distribution_metric: kullback_leibler:
         call"; *)

      let keys_1 = keys t1 in
      let keys_2 = keys t2 in

      let total_nb_occurence_1 = get_total_nb_occurrence t1 in
      let total_nb_occurence_2 = get_total_nb_occurrence t2 in

      assert(total_nb_occurence_1 > 0);
      assert(total_nb_occurence_2 > 0);

      let keys = L.append keys_1 keys_2 in
      let keys = L.unique 
          ~eq: (fun k1 k2 -> Key.compare k1 k2 = 0)
          keys 
      in
      (* let key_set = *)
      (*         L.fold_right *)
      (*           (fun key acc -> *)
      (*             KeySet.add *)
      (*               key *)
      (*               acc *)
      (*           ) *)
      (*           keys *)
      (*           KeySet.empty *)
      (* in *)

      let occurrence_1 =
        L.map
          (fun key ->
             get_key_occurrence
               key
               t1
          )
          keys
      in
      let occurrence_2 =
        L.map
          (fun key ->
             get_key_occurrence
               key
               t2
          )
          keys
      in

      let ratio_1 =
        L.map
          (fun occurrence ->
             let ratio =
               (float_of_int occurrence) 
               /.
               (float_of_int total_nb_occurence_1)
             in

             assert((Batteries.Float.compare ratio nan) <> 0);

             ratio
          )
          occurrence_1
      in
      let ratio_2 =
        L.map
          (fun occurrence ->
             let ratio =
               (float_of_int occurrence) 
               /.
               (float_of_int total_nb_occurence_2)
             in

             assert((Batteries.Float.compare ratio nan) <> 0);

             ratio
          )
          occurrence_2
      in

      let kl_elements =
        L.map2
          (fun ratio_1 ratio_2 ->
             let kl_1 =
               if ratio_2 = 0. then
                 0.
               else
               if ratio_1 = 0. then
                 0.
               else
                 ratio_1 *. (log10 (ratio_1 /. ratio_2))
             in

             let kl_2 =
               if ratio_1 = 0. then
                 0.
               else
               if ratio_2 = 0. then
                 0.
               else
                 ratio_2 *. (log10 (ratio_2 /. ratio_1))
             in

             kl_1 +. kl_2
          )
          ratio_1
          ratio_2
      in

      let kl =
        L.fold_right
          (fun ratio acc -> ratio +. acc)
          kl_elements
          0.
      in

      assert((Batteries.Float.compare kl nan) <> 0);

      (* print_endline "Distribution_metric: kullback_leibler: end"; *)

      kl
    )

  let kullback_leibler_divergence t1 t2 =
    (
      (* print_endline "Distribution_metric: kullback_leibler: call"; *)

      let keys_1 = keys t1 in
      let keys_2 = keys t2 in

      let total_nb_occurence_1 = get_total_nb_occurrence t1 in
      let total_nb_occurence_2 = get_total_nb_occurrence t2 in

      assert(total_nb_occurence_1 > 0);
      assert(total_nb_occurence_2 > 0);

      let keys = L.append keys_1 keys_2 in
      let keys =
        L.unique
          ~eq: (fun k1 k2 -> Key.compare k1 k2 = 0)
          keys
      in

      let occurrence_1 =
        L.map
          (fun key ->
             get_key_occurrence
               key
               t1
          )
          keys
      in
      let occurrence_2 =
        L.map
          (fun key ->
             get_key_occurrence
               key
               t2
          )
          keys
      in

      let ratio_1 =
        L.map
          (fun occurrence ->
             let ratio =
               (float_of_int occurrence)
               /.
               (float_of_int total_nb_occurence_1)
             in

             assert((Batteries.Float.compare ratio nan) <> 0);

             (* print_endline *)
             (*   (sprintf *)
             (*          "ratio 1 %f" *)
             (*          ratio *)
             (*   ); *)

             ratio
          )
          occurrence_1
      in
      let ratio_2 =
        L.map
          (fun occurrence ->
             let ratio =
               (float_of_int occurrence)
               /.
               (float_of_int total_nb_occurence_2)
             in

             assert((Batteries.Float.compare ratio nan) <> 0);

             (* print_endline *)
             (*   (sprintf *)
             (*          "ratio 2 %f" *)
             (*          ratio *)
             (*   ); *)

             ratio
          )
          occurrence_2
      in

      let kl_elements =
        L.map2
          (fun ratio_1 ratio_2 ->
             let value =
               if ratio_2 = 0. then
                 0.
               else
               if ratio_1 = 0. then
                 0.
               else
                 ratio_1 *. (log2 (ratio_1 /. ratio_2))
             in
             (* print_endline *)
             (*   (sprintf *)
             (*          "value %f" *)
             (*          value *)
             (*   ); *)
             value
          )
          ratio_1
          ratio_2
      in

      let kl =
        L.fold_right
          (fun ratio acc -> ratio +. acc)
          kl_elements
          0.
      in

      assert((Batteries.Float.compare kl nan) <> 0);

      (* print_endline "Distribution_metric: kullback_leibler: end"; *)

      kl
    )

  let kullback_leibler_distance t1 t2 =
    (
      let kl1 = kullback_leibler_divergence t1 t2 in
      let kl2 = kullback_leibler_divergence t2 t1 in

      (* if kl1 < 0. then *)
      (*   ( *)
      (*         print_endline *)
      (*           (sprintf *)
      (*              "negative kl for:\n%s\n%s" *)
      (*              (to_string To_string_mode.Normal t1) *)
      (*              (to_string To_string_mode.Normal t2) *)
      (*           ); *)
      (*         assert(false) *)
      (*   ); *)
      (* if kl2 < 0. then *)
      (*   ( *)
      (*         print_endline *)
      (*           (sprintf *)
      (*              "negative kl for:\n%s\n%s" *)
      (*              (to_string To_string_mode.Normal t2) *)
      (*              (to_string To_string_mode.Normal t1) *)
      (*           ); *)
      (*         assert(false) *)
      (*   ); *)
      (* assert(kl1 >= 0.); *)
      (* assert(kl2 >= 0.); *)

      let value =
        (kl1 +. kl2) /. 2.
      in

      let value_old =
        kullback_leibler_old
          t1
          t2
      in

      (* assert(value >= 0.); *)
      (* assert(value <= 1.); *)

      if value < 0. then
        (
          print_endline
            (sprintf
               "jensen shannon (%f + %f = %f) (value_old: %f) < 0:\n%s\n%s"
               kl1
               kl2
               value
               value_old
               (to_string_full t2)
               (to_string_full t1)
            );
          assert(false)
        );

      (* if value > 1. then *)
      (*   ( *)
      (*         print_endline *)
      (*           (sprintf *)
      (*              "jensen shannon (%f + %f = %f) (value_old: %f) > 1:\n%s\n%s" *)
      (*              kl1 *)
      (*              kl2 *)
      (*              value *)
      (*              value_old *)
      (*              (to_string To_string_mode.Normal t2) *)
      (*              (to_string To_string_mode.Normal t1) *)
      (*           ); *)
      (*         assert(false) *)
      (* ); *)

      value
    )

  let jensen_shannon_n weight_t_tuple_list =
    (
      (* PROBLEM: if number of distributions close to occurrence in each
         distribution weighted occurrence = 0, and thus, sum = 0 *)

      (* print_endline *)
      (*   (sprintf *)
      (*      "Distribution: jensen_shannon_n: call" *)
      (*   ); *)

      (* print_endline *)
      (*   (sprintf *)
      (*      "of_ip_group: weight_t_tuple_list:\n%s" *)
      (*      (List_ext.to_string *)
      (*         ~sep: "\n" *)
      (*         (fun (weight, distribution) -> *)
      (*            sprintf *)
      (*              "%f-%s" *)
      (*              weight *)
      (*              (to_string *)
      (*                 To_string_mode.Normal *)
      (*                 distribution *)
      (*              ) *)
      (*         ) *)
      (*         weight_t_tuple_list *)
      (*      ) *)
      (*   ); *)

      (* let length = L.length weight_t_tuple_list in *)

      (* let weight = 1. /. (float_of_int length) in *)

      let weight_float_t_tuple_list =
        L.map
          (fun (weight, t) ->
             weight
             ,
             to_float_t
               t
          )
          weight_t_tuple_list
      in

      let apply_weight (weight, float_t) =
        (
          let l =
            Container_float.bindings
              float_t
          in

          let new_l =
            L.map
              (fun (key, occurrence) ->
                 let new_occurrence =
                   weight *. occurrence
                 in

                 (key, new_occurrence)
              )
              l
          in

          let new_l_filtered =
            L.filter
              (fun (key, occurrence) -> occurrence > 0.)
              new_l
          in

          let float_t_weighted : float Container.t = Container.create 0 in

          L.iter
            (fun (key, occurrences) ->
               Container_float.add_occurrence
                 float_t_weighted
                 key
                 occurrences
               ;

            )
            new_l_filtered
          ;

          float_t_weighted
        )
      in

      let weight_float_t_tuple_list_weighted =
        L.map
          (fun (weight, float_t) ->
             (weight, (apply_weight (weight, float_t)))
          )
          weight_float_t_tuple_list
      in

      let first =
        Container_float.copy (snd (L.hd weight_float_t_tuple_list_weighted))
      in

      let float_t_sum_weighted =
        L.fold_left
          (fun t_acc (_, occurrence) ->
             Container_float.append
               t_acc
               occurrence;

             t_acc
          )
          first
          (L.tl weight_float_t_tuple_list_weighted)
      in

      (* print_endline *)
      (*   (sprintf *)
      (*      "of_ip_group: weight_t_tuple_list_weighted:\n%s" *)
      (*      (List_ext.to_string *)
      (*         ~sep: "\n" *)
      (*         (fun (weight, distribution) -> *)
      (*            sprintf *)
      (*              "%f-%s" *)
      (*              weight *)
      (*              (to_string_float *)
      (*                 To_string_mode.Normal *)
      (*                 distribution *)
      (*              ) *)
      (*         ) *)
      (*         weight_float_t_tuple_list_weighted *)
      (*      ) *)
      (*   ); *)

      (* print_endline *)
      (*   (sprintf *)
      (*      "[Distribution_metric]: jensen_shannon_n: t_sum_weighted:\n%s" *)
      (*      (to_string_float *)
      (*        To_string_mode.Normal *)
      (*        float_t_sum_weighted *)
      (*      ) *)
      (*   ); *)

      let shannon_entropy_float_t_sum_weighted =
        shannon_entropy_normalized_float_list
          (Container_float.bindings float_t_sum_weighted)
      in

      (*       let occurrence_float_list_list = *)
      (*       let p_list_list = *)
      (*         L.map *)
      (* (fun (w, t) -> *)
      (*             let occurrence_list =  *)
      (*               L.map *)
      (*                 snd *)
      (*                 (bindings t) *)
      (*             in *)

      (*             let n = L.length occurrence_list in *)

      (*             let occurrence_float_list = *)
      (*               L.map *)
      (*                 float *)
      (*                 occurrence_list *)
      (*             in *)

      (* occurrence_float_list *)
      (*           ) *)
      (*           weight_t_tuple_list *)
      (*       in *)

      (*             let p_list = *)
      (*               L.map *)
      (*                 (fun occurrence_float -> occurrence_float /. (float n)) *)
      (*                 occurence_float_list *)
      (*             in *)

      (*             p_list *)


      (*       let shannon_entropy_t_sum_weighted__ = *)
      (* shannon_entropy_of_p_list *)

      (*       in *)

      (* print_endline *)
      (*   (sprintf *)
      (*      "Distribution: jensen_shannon_n: shannon_entropy_t_sum_weighted: %f:\nt_sum_weighted: %s" *)
      (*      shannon_entropy_float_t_sum_weighted *)
      (*      (to_string_float *)
      (*         To_string_mode.Normal *)
      (*         float_t_sum_weighted *)
      (*      ) *)
      (*   ); *)

      let shannon_entropy_weighted_list =
        L.map
          (fun (weight, t) ->
             assert(weight > 0.);

             let shannon_entropy = shannon_entropy_normalized t in

             let shannon_entropy_weighted =
               weight
               *.
               shannon_entropy
             in

             (* print_endline *)
             (*   (sprintf *)
             (*          "Distribution: jensen_shannon_n: shannon_entropy: %f * %f = %f:\nt: %s" *)
             (*          weight *)
             (*          shannon_entropy *)
             (*          shannon_entropy_weighted *)
             (*          (to_string *)
             (*             To_string_mode.Normal *)
             (*             t *)
             (*          ) *)
             (*   ); *)

             shannon_entropy_weighted
          )
          weight_t_tuple_list
      in

      let shannon_entropy_weighted_sum =
        L.fold_left
          (fun shannon_entropy_weighted shannon_entropy_weighted_acc ->
             shannon_entropy_weighted_acc +. shannon_entropy_weighted
          )
          (L.hd shannon_entropy_weighted_list)
          (L.tl shannon_entropy_weighted_list)
      in

      (* print_endline *)
      (*   (sprintf *)
      (*      "Distribution: jensen_shannon_n: shannon_entropy_weighted_sum: %f" *)
      (*      shannon_entropy_weighted_sum *)
      (*   ); *)

      let value =
        shannon_entropy_float_t_sum_weighted
        -.
        shannon_entropy_weighted_sum      
      in

      (* print_endline *)
      (*   (sprintf *)
      (*      "Distribution: jensen_shannon_n: jensen-shannon divergence: %f" *)
      (*      value *)
      (*   ); *)

      (* print_endline *)
      (*   (sprintf *)
      (*      "Distribution: jensen_shannon_n: end" *)
      (*   ); *)

      value
    )

  let jensen_shannon_n_length_weight t_list =
    (
      let weight = 1. /. (float_of_int (L.length t_list)) in

      let weight_t_tuple_list =
        L.map
          (fun t ->
             (weight, t)
          )
          t_list
      in

      jensen_shannon_n
        weight_t_tuple_list
    )

  end

  module C_compact = struct

    type t =
      {
        key_array : Key.t array;
        occurrence_array : int array;
      }
    with compare, sexp, bin_io
         
    let new_t
        key_array
        occurrence_array
      =
      {
        key_array;
        occurrence_array;
      }

    let length t = A.length t.key_array
        
    let of_c c =
      let bindings =
        Container.bindings c.C.value_container_hashtable
      in

      let key_array = A.of_list (L.map fst bindings) in
      let occurrence_array = A.of_list (L.map snd bindings) in

      new_t
        key_array
        occurrence_array
        
    let to_c t =
      let bindings =
        A.to_list
          (A.map2
             (fun key occurrence -> (key, occurrence))
             t.key_array
             t.occurrence_array
          )
      in

      C.new_t
        (Container.of_list
           bindings
        )        
      
  end

end
