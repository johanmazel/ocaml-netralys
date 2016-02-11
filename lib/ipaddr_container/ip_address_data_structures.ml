
open Printf

module A = Array_ext
module DA = BatDynArray
module HT = BatHashtbl
module L = BatList

open Sexplib.Std
open Bin_prot.Std
       
open Hashtbl_utils_instantiations
    
let debug_enabled = ref false

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Ip_address_data_structures]: %s@." s)
      else
        ignore
    )
    fmt

module type BIN_INT = sig

  type t

  val compare : t -> t -> int

  val of_int32 : int32 -> t
  val of_int64_tuple : int64 -> int64 -> t
  val to_int : t -> int

  val min : t
  val max : t

  val succ : t -> t
  val pred : t -> t
  val sub : t -> t -> t

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
  
module type UNSIGNED_INT = sig

  module Bin_int : BIN_INT
  
  type t

  val compare : t -> t -> int

  val min : t
  val max : t

  val of_bin_int : Bin_int.t -> t
  val to_bin_int : t -> Bin_int.t
  val to_int : t -> int

  val succ : t -> t
  val pred : t -> t
  val sub : t -> t -> t

  end

module type IP_ADDRESS = sig

  module Bin_int : BIN_INT
  module Unsigned_int : UNSIGNED_INT
    
  type t

  val null : t
  val of_bin_int : Bin_int.t -> t
  val of_unsigned_int : Unsigned_int.t -> t
    
  val to_bin_int : t -> Bin_int.t
  val to_unsigned_int : t -> Unsigned_int.t

  val compare : t -> t -> int
  val to_string : t -> string
  val to_ipaddr : t -> Ipaddr.t

  end

module type PREFIX = sig

  module Ip_address : IP_ADDRESS
    
  type t

  val compare : t -> t -> int

  val to_string : t -> string
  val make : int -> Ip_address.t -> t
  val bits : t -> int
  val bits : t -> int
  val network_address : t -> Ip_address.t -> Ip_address.t
  val network : t -> Ip_address.t
  val broadcast : t -> Ip_address.t
  val mem : Ip_address.t -> t -> bool
    
  val max_size : int

end

module Make
    (Bin_int : BIN_INT)
    (Unsigned_int : UNSIGNED_INT with module Bin_int = Bin_int)
    (Ip_address : IP_ADDRESS with module Bin_int = Bin_int and module Unsigned_int = Unsigned_int)
    (Prefix : PREFIX with module Ip_address = Ip_address)
=
struct
  
  module Bin_int_set = struct
    include Core.Std.Set.Make_binable(
      struct
        type t = Bin_int.t
        with sexp, bin_io

        let compare = Bin_int.compare
        let to_string bin_int =
          Ip_address.to_string
            (Ip_address.of_bin_int
               bin_int
            )
      end
      )

    let cardinal = length
      
    let to_string
        ?sep: (sep = " ")
        t
      =
      let list = elements t in

      List_ext.to_string
        ~sep
        (fun bin_int ->
           Ip_address.to_string
             (Ip_address.of_bin_int
                bin_int
             )
        )
        list

  end
    
  module Bin_int_iset =
    Iset.Make(
    struct
      type t = Bin_int.t
      with sexp, bin_io

      let compare = Bin_int.compare
                      
      let to_string t = Ip_address.to_string (Ip_address.of_bin_int t)

      let hash = Hashtbl.hash

      let min = Bin_int.min
      let max = Bin_int.max

      let pred = Bin_int.pred
      let succ = Bin_int.succ

      let sub t1 t2 = Bin_int.to_int (Bin_int.sub t1 t2)

      let lt t1 t2 = Bin_int.compare t1 t2 < 0
      let gt t1 t2 = Bin_int.compare t1 t2 > 0
      let lte t1 t2 = Bin_int.compare t1 t2 <= 0
      let gte t1 t2 = Bin_int.compare t1 t2 >= 0
      let e t1 t2 = Bin_int.compare t1 t2 = 0
      let ne t1 t2 = Bin_int.compare t1 t2 <> 0

    end
    )
      

  module Unsigned_int_set = struct

    include Set_ext.Make(
      struct
        type t = Unsigned_int.t
        let compare = Unsigned_int.compare
        let to_string unsigned_int =
          Ip_address.to_string
            (Ip_address.of_unsigned_int
               unsigned_int
            )

      end
      )

    let of_bin_int_set bin_int_set =
      let bin_int_l =
        Bin_int_set.to_list
          bin_int_set
      in
      let unsigned_int_l =
        L.map
          Unsigned_int.of_bin_int
          bin_int_l
      in
      of_list
        unsigned_int_l

  end
    
  module Ip_address_set =
    Set_ext.Make(
    struct
      type t = Ip_address.t
      let compare = Ip_address.compare
      let to_string = Ip_address.to_string
    end
    )
  
  module Prefix_set =
    Set_ext.Make(
    struct
      type t = Prefix.t
      let compare = Prefix.compare
      let to_string = Prefix.to_string
      end
    )
  
  module Prefix_utils = struct

    let size t =
      let bits = Prefix.bits t in
      let prefix_size = 2. ** (float_of_int (Prefix.max_size - bits)) in
      int_of_float prefix_size

    let rec common_netmask_between_ipaddr ipaddr1 ipaddr2 netmask =
      match netmask with
      | -1 -> failwith "netmask -1"
      | 0 -> 0
      | _ ->
        (
          let prefix1 = Prefix.make netmask ipaddr1 in
          let prefix2 = Prefix.make netmask ipaddr2 in

          if Prefix.compare prefix1 prefix2 = 0 then
            netmask
          else
            common_netmask_between_ipaddr ipaddr1 ipaddr2 (netmask - 1)
        )

    let common_prefix_between_ipaddr ipaddr1 ipaddr2 =
      let netmask =
        common_netmask_between_ipaddr
          ipaddr1
          ipaddr2
          Prefix.max_size
      in

      Prefix.make netmask ipaddr1


    let common_prefix_betwen_prefix prefix1 prefix2 =
      debug "common_prefix_between_prefix: call";

      let bits1 = Prefix.bits prefix1 in
      let bits2 = Prefix.bits prefix2 in
      let netmask_init_size = min bits1 bits2 in

      let ipaddr1 =
        Prefix.network_address
          prefix1
          Ip_address.null
      in
      let ipaddr2 =
        Prefix.network_address
          prefix2
          Ip_address.null
      in
      let netmask =
        common_netmask_between_ipaddr
          ipaddr1
          ipaddr2
          netmask_init_size
      in

      debug 
        "common_prefix_between_prefix: prefix1: %s ; prefix2: %s"
        (Prefix.to_string prefix1)
        (Prefix.to_string prefix2)
      ;

      debug
        "common_prefix_between_prefix: bits1: %d ; bits2: %d ; netmask_init_size: %d"
        bits1
        bits2
        netmask_init_size
      ;

      debug "common_prefix_between_prefix: prefix: netmask: %d"
        netmask
      ;

      let ipaddr1 =
        Prefix.network_address
          prefix1
          Ip_address.null
      in
      let prefix = Prefix.make netmask ipaddr1 in

      debug "common_prefix_between_prefix: prefix: %s"
        (Prefix.to_string prefix)
      ;

      debug "common_prefix_between_prefix: end";

      prefix

    let prefix_included_2_in_1 prefix prefix_to_test =
      let netmask_size_reference = Prefix.bits prefix in

      let ipaddr_to_test = 
        Prefix.network_address
          prefix_to_test 
          Ip_address.null
      in

      let new_prefix_to_test = 
        Prefix.make
          netmask_size_reference
          ipaddr_to_test
      in

      if Prefix.compare prefix new_prefix_to_test = 0 then
        true
      else
        false

    let prefix_included_12 prefix_1 prefix_2 =
      let netmask_size_1 = Prefix.bits prefix_1 in
      let netmask_size_2 = Prefix.bits prefix_2 in

      let big_prefix, small_prefix =
        if netmask_size_1 < netmask_size_2 then
          prefix_1, prefix_2
        else
          prefix_2, prefix_1
      in

      prefix_included_2_in_1 big_prefix small_prefix

    let prefix_distance prefix_1 prefix_2 =
      (* print_endline "[Ip_address_data_structures]: prefix_distance: call"; *)

      let netmask_size_1 = Prefix.bits prefix_1 in
      let netmask_size_2 = Prefix.bits prefix_2 in

      let d = netmask_size_1 - netmask_size_2 in

      abs d      

  end

  module Sequence = struct

    type t =
      {
        mutable a : Bin_int.t array;
      }
    with compare, sexp, bin_io

    let new_t
        a
      =
      {
        a;
      }

    let new_empty_t _ =
      new_t
        [| |]

    let to_string t =
      (
        let bin_int_to_ipaddr bin_int =
          Ip_address.to_string
            (Ip_address.of_bin_int
               bin_int
            )
        in

        let string =
          A.to_string
            ~sep: " "
            bin_int_to_ipaddr
            t.a
        in

        string
      )

    let length t =
      A.length t.a

    let to_ipaddr_array t =
      A.map
        Ip_address.of_bin_int
        t.a
      
    let of_ipaddr_array a =
      new_t
        (A.map
           Ip_address.to_bin_int
           a
        )
      
  end
  
  module Container_compact = struct

    type t =
      {
        mutable isolated : Bin_int.t array;

        mutable iset : Bin_int_iset.t;
      }
    with compare, sexp, bin_io

    let new_t
        isolated
        iset
      =
      {
        isolated;
        iset;
      }

    let new_empty_t _ =
      new_t
        [| |]
        (Bin_int_iset.empty)

    let extract_isolated (iset : Bin_int_iset.t)  =
      let isolated_addr_list =
        Bin_int_iset.fold_range
          (fun ipaddr_l ipaddr_h list_acc ->
             if Bin_int.compare ipaddr_l ipaddr_h = 0 then
               L.append [ ipaddr_l ] list_acc
             else
               list_acc
          )
          iset
          []
      in

      let new_iset : Bin_int_iset.t =
        L.fold_left
          (fun iset_acc ipaddr ->
             Bin_int_iset.remove_range
               ipaddr
               ipaddr
               iset_acc
          )
          iset
          isolated_addr_list
      in

      (isolated_addr_list, new_iset)

    let of_list list =
      (
        let iset =
          L.fold_left
            (fun iset_acc bin_int ->
               Bin_int_iset.add
                 bin_int
                 iset_acc
            )
            Bin_int_iset.empty
            list
        in
        let (isolated_addr_list, new_iset) = extract_isolated iset in

        new_t
          (A.of_list isolated_addr_list)
          new_iset
      )

    let to_list t =
      debug "Container_compact: to_list: call";

      debug "Container_compact: to_list: isolated_list";
      let isolated_list = A.to_list t.isolated in

      debug "Container_compact: to_list: value_list";
      let value_list = Bin_int_iset.elements t.iset in

      debug "Container_compact: to_list: append";
      let l = L.append isolated_list value_list in

      debug "Container_compact: to_list: end";

      l

    let to_string t =
      (
        let bin_int_to_ipaddr bin_int =
          Ip_address.to_string
            (Ip_address.of_bin_int
               bin_int
            )
        in

        let isolated_list = A.to_list t.isolated in
        let isolated_string =
          Bin_int_set.to_string
            ~sep: " "
            (Bin_int_set.of_list
               isolated_list
            )
        in

        let range_list =
          Bin_int_iset.ranges
            t.iset
        in
        let range_string_list =
          L.map
            (fun (bin_int_1, bin_int_2) ->
               bin_int_to_ipaddr bin_int_1,
               bin_int_to_ipaddr bin_int_2               
            )
            range_list
        in
        let range_string =
          List_ext.to_string
            (fun (string_1, string_2) ->
               string_1 ^ "-" ^ string_2
            )
            range_string_list
        in

        isolated_string ^ "\n" ^ range_string
      )

    let length t =
      (A.length t.isolated) + (Bin_int_iset.cardinal t.iset)

    let consecutive t =
      new_t
        [| |]
        t.iset     

    let consecutiveness_number t =
      Bin_int_iset.cardinal t.iset

    let consecutiveness_ratio t =
      if length t = 1 then
        1.0
      else
        (float_of_int (consecutiveness_number t))
        /.
        (float_of_int (length t))

    let inter t1 t2 =
      let iset_1_complete =
        A.fold_left
          (fun iset_acc ipaddr ->
             Bin_int_iset.add
               ipaddr
               iset_acc
          )
          t1.iset
          t1.isolated
      in

      let iset_2_complete =
        A.fold_left
          (fun iset_acc ipaddr ->
             Bin_int_iset.add
               ipaddr
               iset_acc
          )
          t2.iset
          t2.isolated
      in

      let iset_inter =
        Bin_int_iset.inter
          iset_1_complete
          iset_2_complete
      in

      let (isolated_addr_list, new_iset) = extract_isolated iset_inter in

      new_t
        (A.of_list isolated_addr_list)
        new_iset

    let inter t1 t2 =
      let l1 =
        to_list
          t1
      in
      let l2 =
        to_list
          t2
      in

      let s1 = Bin_int_set.of_list l1 in
      let s2 = Bin_int_set.of_list l2 in
      let inter = Bin_int_set.inter s1 s2 in

      of_list
        (Bin_int_set.to_list
           inter
        )

    let union t1 t2 =
      let iset_1_complete =
        A.fold_left
          (fun iset_acc ipaddr ->
             Bin_int_iset.add
               ipaddr
               iset_acc
          )
          t1.iset
          t1.isolated
      in

      let iset_2_complete =
        A.fold_left
          (fun iset_acc ipaddr ->
             Bin_int_iset.add
               ipaddr
               iset_acc
          )
          t2.iset
          t2.isolated
      in

      let iset_union =
        Bin_int_iset.union
          iset_1_complete
          iset_2_complete
      in

      let (isolated_addr_list, new_iset) = extract_isolated iset_union in

      new_t
        (A.of_list isolated_addr_list)
        new_iset

    let iter f t =
      A.iter
        f
        t.isolated;

      Bin_int_iset.iter
        f
        t.iset;

  end

  module Container = struct

    module P = Prefix
      
    type t =
      {
        data : Unsigned_int_set.t;
      }
    with compare

    let new_t
        data
      =
      {
        data;
      }

    let new_empty_t () = new_t Unsigned_int_set.empty

    let to_string t =
      Unsigned_int_set.to_string
        ~sep: " "
        t.data

    let of_list l =
      new_t
        (Unsigned_int_set.of_list l)

    let to_list t = 
      Unsigned_int_set.to_list t.data

    let to_ipaddr_list t =
      let unsigned_int_l = to_list t in
      L.map
        (fun unsigned_int -> Ip_address.to_ipaddr (Ip_address.of_unsigned_int unsigned_int))
        unsigned_int_l
        
    let of_ip_address_container_compact
        ip_address_container_compact
      =
      let bin_int_l = Container_compact.to_list ip_address_container_compact in

      let unsigned_int_l =
        L.map
          Unsigned_int.of_bin_int
          bin_int_l
      in

      let t =        
        of_list
          unsigned_int_l
      in

      t
      
    let to_ip_address_container_compact
        t
      =
      Container_compact.of_list
        (L.map
           Unsigned_int.to_bin_int
           (to_list t)
        )
        
    let is_empty t = Unsigned_int_set.is_empty t.data

    let length t = Unsigned_int_set.cardinal t.data
        
    let fold_right_ipaddr
        f
        t
        acc
      =
      Unsigned_int_set.fold
        (fun unsigned_int acc ->
           f
             acc
             (Ip_address.to_ipaddr (Ip_address.of_unsigned_int unsigned_int))
        )
        t.data
        acc

    let filter_prefix
        prefix
        t
      =
      new_t
        (Unsigned_int_set.filter
           (fun uint ->
              let ip_address = Ip_address.of_unsigned_int uint in
              Prefix.mem ip_address prefix
           )
           t.data
        )
        
    let iter
        f
        t
      =
      Unsigned_int_set.iter
        f
        t.data

    let inter t1 t2 =
      new_t
        (Unsigned_int_set.inter t1.data t2.data)

    let union t1 t2 =
      new_t
        (Unsigned_int_set.union t1.data t2.data)

    let jaccard t1 t2 =
      let inter = Unsigned_int_set.inter t1.data t2.data in
      let inter_length = Unsigned_int_set.cardinal inter in
      let inter_length_float = float_of_int inter_length in

      let union = Unsigned_int_set.union t1.data t2.data in
      let union_length = Unsigned_int_set.cardinal union in
      let union_length_float = float_of_int union_length in

      let jaccard =
        inter_length_float
        /. 
        union_length_float
      in

      jaccard

    let consecutive_number_fold_succ t =
      let _, consecutive_number =
        Unsigned_int_set.fold_succ
          (fun previous_unsigned_int unsigned_int (previous_is_consecutive, acc) ->
             if Unsigned_int.compare (Unsigned_int.succ previous_unsigned_int) unsigned_int = 0 then
               (
                 if previous_is_consecutive then
                   (true, (acc + 1))
                 else
                   (true, acc + 2)
               )
             else
               (false, acc)
          )
          t.data
          (false, 0)
      in
      consecutive_number

    let consecutive_number = consecutive_number_fold_succ

    let do_while f p ~init =
      let rec loop v =
        let v = f v in
        if p v then loop v
      in
      loop init

    let prefix t =
      debug "Container: prefix: call";

      debug
        "Container: prefix: t:\n%s"
        (to_string
           t
        );

      let result =
        if Unsigned_int_set.cardinal t.data = 0 then
          Prefix.make
            Prefix.max_size
            Ip_address.null
        else
          let first_ipaddr = Unsigned_int_set.min_elt t.data in
          let other_ipaddr_set = Unsigned_int_set.remove first_ipaddr t.data in

          let first_ip_address =
            Ip_address.of_unsigned_int
              first_ipaddr          
          in

          debug "Container: prefix: t length: %d" (Unsigned_int_set.cardinal t.data);

          debug "Container: prefix: building prefix with prefix intersection";

          let prefix =
            Unsigned_int_set.fold
              (fun unsigned_int prefix_acc ->
                 let ip_address =
                   Ip_address.of_unsigned_int
                     unsigned_int
                 in

                 debug
                   "Container: prefix: ip_address: %s ; prefix_acc: %s"
                   (Ip_address.to_string
                      ip_address
                   )
                   (Prefix.to_string
                      prefix_acc
                   );

                 if Prefix.mem ip_address prefix_acc then
                   prefix_acc
                 else
                   let prefix_ip_address =
                     Prefix.network_address
                       prefix_acc
                       Ip_address.null
                   in

                   debug
                     "Container: prefix: prefix ip_address: %s"
                     (Ip_address.to_string
                        prefix_ip_address
                     );

                   let common_prefix =
                     Prefix_utils.common_prefix_between_ipaddr
                       ip_address
                       prefix_ip_address
                   in

                   debug
                     "Container: prefix: common prefix: %s"
                     (Prefix.to_string
                        common_prefix
                     );

                   common_prefix
              )
              other_ipaddr_set
              (Prefix.make
                 Prefix.max_size
                 first_ip_address
              )
          in

          debug "Container: prefix: prefix: %s" (Prefix.to_string prefix);

          prefix
      in

      debug "Container: prefix: end";

      result

    let prefix_old t =
      debug "Container: prefix: call";

      debug
        "Container: prefix: t:\n%s"
        (to_string
           t
        );

      let result =
        if Unsigned_int_set.cardinal t.data = 0 then
          Prefix.make
            Prefix.max_size
            Ip_address.null
        else
          let first_ipaddr = Unsigned_int_set.min_elt t.data in
          let other_ipaddr_set = Unsigned_int_set.remove first_ipaddr t.data in

          let first_ip_address =
            Ip_address.of_unsigned_int
              first_ipaddr          
          in

          debug "Container: prefix: t length: %d" (Unsigned_int_set.cardinal t.data);

          debug "Container: prefix: building prefix with prefix intersection";

          let prefix =
            Unsigned_int_set.fold
              (fun unsigned_int prefix_acc ->
                 let ip_address =
                   Ip_address.of_unsigned_int
                     unsigned_int
                 in

                 debug
                   "Container: prefix: ip_address: %s ; prefix_acc: %s"
                   (Ip_address.to_string
                      ip_address
                   )
                   (Prefix.to_string
                      prefix_acc
                   );

                 let prefix_ip_address =
                   Prefix.network_address
                     prefix_acc
                     Ip_address.null
                 in

                 debug
                   "Container: prefix: prefix ip_address: %s"
                   (Ip_address.to_string
                      prefix_ip_address
                   );

                 let common_prefix =
                   Prefix_utils.common_prefix_between_ipaddr
                     ip_address
                     prefix_ip_address
                 in

                 debug
                   "Container: prefix: common prefix: %s"
                   (Prefix.to_string
                      common_prefix
                   );

                 common_prefix
              )
              other_ipaddr_set
              (Prefix.make
                 Prefix.max_size
                 first_ip_address
              )
          in

          debug "Container: prefix: prefix: %s" (Prefix.to_string prefix);

          prefix
      in

      debug "Container: prefix: end";

      result

    let prefix_list size t =
      let prefix_set_init = Prefix_set.empty in

      let prefix_set =
        Unsigned_int_set.fold
          (fun ipaddr prefix_set_acc ->
             let ip_address =
               Ip_address.of_unsigned_int ipaddr
             in
             let prefix = Prefix.make size ip_address in
             Prefix_set.add
               prefix
               prefix_set_acc
          )
          t.data
          prefix_set_init
      in

      Prefix_set.to_list prefix_set

    let number_prefix size t =
      L.length
        (prefix_list size t)
        
    (* let number_prefix size t = *)
    (*   let prefix_set_init = Prefix_set.empty in *)

    (*   let prefix_set = *)
    (*     Unsigned_int_set.fold *)
    (*       (fun ipaddr prefix_set_acc -> *)
    (*          let ip_address = *)
    (*            Ip_address.of_unsigned_int ipaddr *)
    (*          in *)
    (*          let prefix = Prefix.make size ip_address in *)
    (*          Prefix_set.add *)
    (*            prefix *)
    (*            prefix_set_acc *)
    (*       ) *)
    (*       t.data *)
    (*       prefix_set_init *)
    (*   in *)

    (*   Prefix_set.cardinal prefix_set *)

    (* Deprecated *)
    let number_24_prefix t =
      let prefix_set_init = Prefix_set.empty in

      let prefix_set =
        Unsigned_int_set.fold
          (fun uint prefix_set_acc ->
             let ip_address =
               Ip_address.of_unsigned_int uint
             in
             let prefix = Prefix.make 24 ip_address in
             Prefix_set.add
               prefix
               prefix_set_acc
          )
          t.data
          prefix_set_init
      in

      Prefix_set.cardinal prefix_set

    let nth_bit_zero bit_indice t =
      debug "nth_bit_zero: call";
      
      assert(bit_indice > 0);
      let unsigned_int_set_zero =
        Unsigned_int_set.fold
          (fun uint acc ->
             let ip_address = Ip_address.of_unsigned_int uint in

             let prefix_small = Prefix.make (bit_indice - 1) ip_address in
             let prefix_current = Prefix.make bit_indice ip_address in

             let network_small = Prefix.network prefix_small in
             let network_current = Prefix.network prefix_current in

             let diff =
               Unsigned_int.sub
                 (Ip_address.to_unsigned_int network_current)
                 (Ip_address.to_unsigned_int network_small)
             in

             debug
               "nth_bit_zero: ip_address: %s ; bit_indice: %d ; prefix_small: %s ; prefix_current: %s"
               (Ip_address.to_string ip_address)
               bit_indice
               (Prefix.to_string prefix_small)
               (Prefix.to_string prefix_current)
             ;
             debug
               "nth_bit_zero: network_small: %s ; network_current: %s ; diff: %d"
               (Ip_address.to_string network_small)
               (Ip_address.to_string network_current)
               (Unsigned_int.to_int diff)
             ;

             let is_zero =
               Unsigned_int.compare
                 diff
                 (Unsigned_int.of_bin_int
                    (Bin_int.of_int32
                       Int32.zero
                    )
                 )
               =
               0
             in

             if is_zero then
               Unsigned_int_set.add
                 uint
                 acc
             else
               acc
          )
          t.data
          Unsigned_int_set.empty
      in

      let r :t =
        new_t
          unsigned_int_set_zero
      in

      debug "nth_bit_zero: end";
      
      r
    
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

    let mean_med_array1_gsl_sort
        list
      =
      let array =
        Batteries.Array.of_list
          list
      in
      let array =
        Batteries.Array.map
          float_of_int
          array
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
      let med_int = int_of_float med in

      let _90centile =
        Gsl.Stats.quantile_from_sorted_data
          array_sorted
          0.9
      in
      let _90centile_int = int_of_float _90centile in

      let _95centile =
        Gsl.Stats.quantile_from_sorted_data
          array_sorted
          0.95
      in
      let _95centile_int = int_of_float _95centile in

      let _99centile =
        Gsl.Stats.quantile_from_sorted_data
          array_sorted
          0.99
      in
      let _99centile_int = int_of_float _99centile in

      let _max =
        Gsl.Stats.max
          array_sorted
      in
      let _max_int = int_of_float _max in

      (mean, med_int, _90centile_int, _95centile_int, _99centile_int, _max_int)

    let get_consecutiveness_empty_run_length_std_mean_med_centiles_max
        t
      =
      let _, consecutive_number, empty_run_length_list =
        Unsigned_int_set.fold_succ
          (fun previous_unsigned_int unsigned_int (previous_is_consecutive, consecutive_number_acc, empty_run_length_list_acc) ->
             assert(Unsigned_int.compare previous_unsigned_int unsigned_int < 0);

             let empty_run_length =
                 Unsigned_int.sub unsigned_int previous_unsigned_int
             in
             let empty_run_length_int = Unsigned_int.to_int empty_run_length in

             let new_empty_run_length_list_acc =
               empty_run_length_int :: empty_run_length_list_acc
             in

             let unsigned_int_consecutive = Unsigned_int.compare (Unsigned_int.succ previous_unsigned_int) unsigned_int = 0 in

             if unsigned_int_consecutive then
               (
                 if previous_is_consecutive then
                   (true, consecutive_number_acc + 1, new_empty_run_length_list_acc)
                 else
                   (true, consecutive_number_acc + 2, new_empty_run_length_list_acc)
               )
             else
               (false, consecutive_number_acc, new_empty_run_length_list_acc)
          )
          t.data
          (false, 0, [])
      in

      let consecutiveness_ratio =  
        if length t = 1 then
          1.0
        else
          (float_of_int consecutive_number)
          /.
          (float_of_int (length t))
      in

      let mean, med, _90centile, _95centile, _99centile, max =
        mean_med_array1_gsl_sort
          empty_run_length_list
      in
      let sd =
        Gsl.Stats.sd
          (A.of_list
             (L.map
                float_of_int
                empty_run_length_list
             )
          )
      in
      
      (consecutiveness_ratio, (sd, mean, med), (_90centile, _95centile, _99centile, max))

    let coverage_min_max
        t
      =
      debug "coverage: call";

      let min = Unsigned_int_set.min_elt t.data in
      let max = Unsigned_int_set.max_elt t.data in

      let min_ipaddr =
        Ip_address.of_unsigned_int
          min
      in
      let max_ipaddr =
        Ip_address.of_unsigned_int
          max
      in

      debug
        "coverage: min: %s ; max: %s"
        (Ip_address.to_string min_ipaddr)
        (Ip_address.to_string max_ipaddr)
      ;

      let total_possible_size = Unsigned_int.to_int (Unsigned_int.sub max min) + 1 in

      debug
        "coverage: length: %d ; total_possible_size: %d"
        (length t)
        total_possible_size
      ;

      let coverage =
        (float_of_int (length t))
        /.
        (float_of_int total_possible_size)
      in

      assert(coverage <= 1.);

      debug "coverage: end";

      coverage

    let coverage_min_max_prefix
        prefix
        t
      =
      debug "coverage_min_max_prefix: call";

      let min = Unsigned_int_set.min_elt t.data in
      let max = Unsigned_int_set.max_elt t.data in

      let min_ipaddr =
        Ip_address.of_unsigned_int
          min
      in
      let max_ipaddr =
        Ip_address.of_unsigned_int
          max
      in

      debug
        "coverage_min_max_prefix: prefix: %s ; min: %s ; max: %s"
        (Prefix.to_string prefix)
        (Ip_address.to_string min_ipaddr)
        (Ip_address.to_string max_ipaddr)
      ;

      let total_possible_size = Unsigned_int.to_int (Unsigned_int.sub max min) in

      debug "coverage_min_max_prefix: prefix: %s" (Prefix.to_string prefix);

      debug
        "coverage_min_max_prefix: network: %s ; broadcast: %s"
        (Ip_address.to_string (Prefix.network prefix))
        (Ip_address.to_string (Prefix.broadcast prefix))
      ;

      let prefix_network_bits = Prefix.bits prefix in
      let prefix_size = 2. ** (float_of_int (32 - prefix_network_bits)) in

      let total_possible_size_float =  float_of_int total_possible_size in

      debug
        "coverage_min_max_prefix: total_possible_size: %d ; prefix_size: %f"
        total_possible_size
        prefix_size
      ;

      assert(total_possible_size_float <= prefix_size);

      let coverage_in_prefix =
        (total_possible_size_float)
        /.
        prefix_size
      in

      assert(coverage_in_prefix <= 1.);

      debug "coverage_min_max_prefix: end";

      coverage_in_prefix

    let coverage_prefix
        prefix
        t
      =
      debug "coverage_prefix: call";

      debug
        "coverage_prefix: prefix: %s"
        (Prefix.to_string prefix)
      ;

      debug
        "coverage_prefix: network: %s ; broadcast: %s"
        (Ip_address.to_string (Prefix.network prefix))
        (Ip_address.to_string (Prefix.broadcast prefix))
      ;

      let prefix_network_bits = Prefix.bits prefix in
      let prefix_size = 2. ** (float_of_int (32 - prefix_network_bits)) in

      let length_float = float_of_int (length t) in

      debug
        "coverage_prefix: length: %d ; prefix_size: %f"
        (length t)
        prefix_size
      ;

      assert(length_float <= prefix_size);

      let coverage_prefix =
        length_float
        /.
        prefix_size
      in

      assert(coverage_prefix <= 1.);

      debug "coverage_prefix: end";

      coverage_prefix

    let extract_smallest_biggest_part_around_biggest_gap
        t
      =
      debug "extract_biggest_half_from_biggest_gap: call";

      debug
        "extract_biggest_half_from_biggest_gap: ipaddr set length: %d"
        (Unsigned_int_set.cardinal
           t.data
        )
      ;

      let set_start =
        Ip_address.to_string
          (Ip_address.of_unsigned_int
             (Unsigned_int_set.min_elt t.data)
          )
      in
      let set_end = 
        Ip_address.to_string
          (Ip_address.of_unsigned_int
             (Unsigned_int_set.max_elt t.data)
          )
      in

      debug
        "extract_biggest_half_from_biggest_gap: from %s to %s"
        set_start
        set_end
      ;

      let ple_list =
        Unsigned_int_set.fold_succ
          (fun previous_unsigned_int unsigned_int ple_list_acc ->
             assert(Unsigned_int.compare previous_unsigned_int unsigned_int < 0);

             let empty_run_length =
               let sub =
                 Unsigned_int.sub unsigned_int previous_unsigned_int
               in
               sub
             in
             let empty_run_length_int = Unsigned_int.to_int empty_run_length in

             assert(empty_run_length_int > 0);

             if empty_run_length_int = 1 then
               ple_list_acc
             else
               (empty_run_length_int, previous_unsigned_int, unsigned_int) :: ple_list_acc
          )
          t.data
          []
      in

      debug
        "extract_biggest_half_from_biggest_gap: ple_list length: %d"
        (L.length
           ple_list
        )
      ;

      let m_multi =
        Core.Std.Int.Map.of_alist_multi
          (L.map
             (fun (empty_run_length_int, previous_unsigned_int, unsigned_int) ->
                (empty_run_length_int, (previous_unsigned_int, unsigned_int))
             )
             ple_list
          )
      in

      let m =
        Core.Std.Int.Map.map
          m_multi
          (fun data -> L.hd data)
      in

      debug "extract_biggest_half_from_biggest_gap: m length: %d" (Core.Std.Int.Map.length m);

      let (biggest_erl, (previous_unsigned_int, unsigned_int)) =
        Core.Std.Int.Map.max_elt_exn
          m
      in

      debug
        "extract_biggest_half_from_biggest_gap: biggest_erl: %d ; between %s and %s"
        biggest_erl
        (Ip_address.to_string (Ip_address.of_unsigned_int previous_unsigned_int))
        (Ip_address.to_string (Ip_address.of_unsigned_int unsigned_int))
      ;

      debug "extract_biggest_half_from_biggest_gap: split_lt";
      let left, present, right =
        Unsigned_int_set.split
          unsigned_int
          t.data
      in

      let right =
        if present then
          Unsigned_int_set.add
            unsigned_int
            right
        else
          right
      in

      let small_set, big_set =
        if Unsigned_int_set.cardinal right > Unsigned_int_set.cardinal left then
          left, right
        else
          right, left
      in

      debug
        "extract_biggest_half_from_biggest_gap: small_set length: %d ; big_set length: %d"
        (Unsigned_int_set.cardinal small_set)
        (Unsigned_int_set.cardinal big_set)
      ;

      let small_set_start =
        Ip_address.to_string
          (Ip_address.of_unsigned_int
             (Unsigned_int_set.min_elt small_set)
          )
      in
      let small_set_end = 
        Ip_address.to_string
          (Ip_address.of_unsigned_int
             (Unsigned_int_set.max_elt small_set)
          )
      in
      let big_set_start = 
        Ip_address.to_string
          (Ip_address.of_unsigned_int
             (Unsigned_int_set.min_elt big_set)
          )
      in
      let big_set_end =
        Ip_address.to_string
          (Ip_address.of_unsigned_int
             (Unsigned_int_set.max_elt big_set)
          )
      in

      debug
        "extract_biggest_half_from_biggest_gap: small_set from %s to %s ; big_set from %s to %s"
        small_set_start
        small_set_end
        big_set_start
        big_set_end
      ;

      debug "extract_biggest_half_from_biggest_gap: end";

      new_t small_set, new_t big_set

    let remove_borders border_percentage t =
      let array =
        A.of_list
          (Unsigned_int_set.to_list
             t.data
          )
      in
      let length = A.length array in
      let number_of_elements_to_remove = int_of_float (border_percentage *. (float_of_int length)) in

      let set = Unsigned_int_set.of_list (A.to_list array) in
      let dummy_array = A.make number_of_elements_to_remove 0 in
      let set_without_first =
        A.fold_right
          (fun _ s ->
             let min_elt = Unsigned_int_set.min_elt s in
             Unsigned_int_set.remove
               min_elt
               s
          )
          dummy_array
          set
      in
      let set_without_first_last =
        A.fold_right
          (fun _ s ->
             let max_elt = Unsigned_int_set.max_elt s in
             Unsigned_int_set.remove
               max_elt
               s
          )
          dummy_array
          set_without_first
      in

      new_t
        set_without_first_last

    let prefix_max_size =
      Prefix.max_size
    
  end

  module Container_special = struct

    module Int_core_ht = Core_kernel.Core_hashtbl.Make_binable(
      struct
        type t = int
        with compare, sexp, bin_io

        let hash = Hashtbl.hash

        (* let compare = compare *)
        (* let to_string bin_int = *)
        (*   Ip_address.to_string *)
        (*     (Ip_address.of_bin_int *)
        (*        bin_int *)
        (*     ) *)
      end
      )
    
    type subset_h = Bin_int_set.t Int_core_ht.t
    with sexp, bin_io

    let compare_subset_h t1 t2 =
      BatHashtbl_utils.compare
        Bin_int_set.compare
        (BatHashtbl.of_enum (L.enum (Int_core_ht.to_alist t1)))
        (BatHashtbl.of_enum (L.enum (Int_core_ht.to_alist t2)))

    type subset_element_set_h = Core_kernel.Core_int.Set.t Int_core_ht.t
    with sexp, bin_io

    let compare_subset_element_set_h t1 t2 =
      BatHashtbl_utils.compare      
        Core_kernel.Core_int.Set.compare
        (BatHashtbl.of_enum (L.enum (Int_core_ht.to_alist t1)))
        (BatHashtbl.of_enum (L.enum (Int_core_ht.to_alist t2)))

    type element_subset_set_h = Core_kernel.Core_int.Set.t Int_core_ht.t
    with sexp, bin_io

    let compare_element_subset_set_h t1 t2 =
      BatHashtbl_utils.compare
        Core_kernel.Core_int.Set.compare
        (BatHashtbl.of_enum (L.enum (Int_core_ht.to_alist t1)))
        (BatHashtbl.of_enum (L.enum (Int_core_ht.to_alist t2)))

    type t =
      {
        mutable current_subset_indice : int;

        subset_h : subset_h;

        subset_element_set_h : subset_element_set_h;

        element_subset_set_h : element_subset_set_h;
      }
    with compare, sexp, bin_io

    let new_t
        current_subset_indice

        subset_h

        subset_element_set_h

        element_subset_set_h
      =
      {
        current_subset_indice;

        subset_h;

        subset_element_set_h;

        element_subset_set_h;
      }

    let new_empty_t () =
      new_t
        0
        (Int_core_ht.create ())
        (Int_core_ht.create ())
        (Int_core_ht.create ())

    let to_string_int_set set =
      List_ext.to_string
        string_of_int
        (Core_kernel.Core_int.Set.to_list set)

    let to_string_bin_int_set set =
      Bin_int_set.to_string set

    let to_string t =
      sprintf
        "Ip_address_special_container: current_subset_indice %d\nsubset_h:\n%s\nsubset_element_set_h:\n%s\nelement_subset_set_h:\n%s"
        t.current_subset_indice
        (List_ext.to_string
           ~sep: " ; "
           (fun (int, bin_int_set) ->
              sprintf
                "%d: %s"
                int
                (to_string_bin_int_set bin_int_set)
           )
           (Int_core_ht.to_alist t.subset_h)
        )
        (List_ext.to_string
           ~sep: " ; "
           (fun (int, int_set) ->
              sprintf
                "%d: %s"
                int
                (to_string_int_set int_set)
           )
           (Int_core_ht.to_alist t.subset_element_set_h)
        )
        (List_ext.to_string
           ~sep: " ; "
           (fun (int, int_set) ->
              sprintf
                "%d: %s"
                int
                (to_string_int_set int_set)
           )
           (Int_core_ht.to_alist t.element_subset_set_h)
        )

    let remove_subset_indice
        t
        subset_indice
      =
      (
        (* remove subset in subset_h *)
        Int_core_ht.remove
          t.subset_h
          subset_indice;

        (* remove each subset indice in element_subset_set_h *)
        let element_set_found =
          Int_core_ht.find_exn
            t.subset_element_set_h
            subset_indice
        in
        Core_kernel.Core_int.Set.iter
          element_set_found
          (fun element ->
             let subset_set_found =
               Int_core_ht.find_exn
                 t.element_subset_set_h
                 element
             in

             let new_subset_set =
               Core_kernel.Core_int.Set.remove
                 subset_set_found
                 subset_indice
             in

             (* if Int_set.cardinal new_subset_set = 0 then *)
             if Core_kernel.Core_int.Set.is_empty new_subset_set then
               Int_core_ht.remove
                 t.element_subset_set_h
                 element        
             else
               Int_core_ht.replace
                 t.element_subset_set_h
                 element
                 new_subset_set
          )
        ;

        (* remove subset indice in subset_element_set_h *)
        Int_core_ht.remove
          t.subset_element_set_h
          subset_indice;

        element_set_found
      )

    let add_subset
        t
        element_set
        subset
      =
      (
        if Bin_int_set.cardinal subset = 0 then
          (
            print_endline
              (sprintf
                 "\n\n[Ip_address_special_container]: add_subset: adding empty subset for elements:\n%s\n%s"
                 (to_string_int_set element_set)
                 (to_string t)
              );
            assert(false);
          );

        let subset_indice = t.current_subset_indice in
        t.current_subset_indice <- t.current_subset_indice+1;

        (* add subset in subset_h *)
        Int_core_ht.add_exn
          t.subset_h
          subset_indice
          subset;

        (* add element indice set for subset indice in subset_element_set_h *)
        Int_core_ht.add_exn
          t.subset_element_set_h
          subset_indice
          element_set;

        (* add subset indice for each element indice in element_subset_set_h *)
        Core_kernel.Core_int.Set.iter
          element_set
          (fun element ->
             try
               let subset_set_found =
                 Int_core_ht.find_exn
                   t.element_subset_set_h
                   element
               in

               assert(Core_kernel.Core_int.Set.mem subset_set_found subset_indice = false);

               let new_subset_set =
                 Core_kernel.Core_int.Set.add
                   subset_set_found
                   subset_indice
               in

               Int_core_ht.replace
                 t.element_subset_set_h
                 element
                 new_subset_set;
             with
             | Not_found ->
               (
                 let new_subset_set =
                   Core_kernel.Core_int.Set.singleton
                     subset_indice
                 in

                 Int_core_ht.add_exn
                   t.element_subset_set_h
                   element
                   new_subset_set;          
               )
          )
        ;
      )

    let add
        t
        element_indice
        bin_int_set_to_add
      =
      (
        debug "\n\n";
        debug "add: call";

        assert(Bin_int_set.cardinal bin_int_set_to_add > 0);

        let new_subset_h =
          Int_core_ht.copy
            t.subset_h
        in

        (* try to add bin_int_set to each element in hashtable *)
        let bin_int_set_remaining_after_adding : Bin_int_set.t = 
          Int_core_ht.fold
            new_subset_h
            ~f: (fun ~key: subset_indice ~data: bin_int_subset bin_int_set_to_add_remaining ->
               debug "add: folding on subset indice %d" subset_indice;

               let inter_subset =
                 Bin_int_set.inter
                   bin_int_set_to_add_remaining
                   bin_int_subset
               in

               if Bin_int_set.cardinal inter_subset = 0 then
                 (
                   debug "add: no intersection";

                   bin_int_set_to_add_remaining
                 )
               else
                 (
                   if Bin_int_set.compare bin_int_subset bin_int_set_to_add_remaining = 0 then
                     (
                       assert(Bin_int_set.equal bin_int_subset bin_int_set_to_add_remaining);
                       (* we have complete intersection with this element: *)

                       (* remove previous intersecting subset indice from hashtables and get element set *)
                       let previous_element_set =
                         remove_subset_indice
                           t
                           subset_indice
                       in

                       (* debug *)
                       (*   "add: t after removing intersecting subset:\n%s" *)
                       (*   (to_string t) *)
                       (* ; *)

                       (* add new element indice to previous element set *)
                       let new_element_set =
                         Core_kernel.Core_int.Set.add
                           previous_element_set
                           element_indice
                       in

                       (* add new element with intersecting subset *)
                       add_subset
                         t
                         new_element_set
                         inter_subset;

                       (* debug *)
                       (*   "add: t after adding intersecting subset for all elements:\n%s" *)
                       (*   (to_string t) *)
                       (* ; *)

                       Bin_int_set.empty
                     )
                   else
                     (
                       (* we have partial intersection with this element: *)

                       (* remove previous intersecting subset indice from hashtables and get element *)
                       let previous_element_set =
                         remove_subset_indice
                           t
                           subset_indice
                       in

                       (* build non-intersecting subset from previous subset *)
                       let diff_subset_from_previous =
                         Bin_int_set.diff
                           bin_int_subset
                           inter_subset
                       in

                       (* if Bin_int_set.cardinal diff_subset_from_previous > 0 then *)
                       if Bin_int_set.is_empty diff_subset_from_previous = false then
                         (
                           (* add non-intersecting subset from previous subset *)
                           add_subset
                             t
                             previous_element_set
                             diff_subset_from_previous;

                           (* debug *)
                           (*       "add: t after adding non-intersecting subset from previous elements:\n%s" *)
                           (*       (o_string t); *)
                         );


                       (* add new element indice to previous element set *)
                       let new_element_set =
                         Core_kernel.Core_int.Set.add
                           previous_element_set
                           element_indice
                       in

                       (* add new element with intersecting subset *)
                       add_subset
                         t
                         new_element_set
                         inter_subset;

                       (* debug *)
                       (*   "add: t after adding intersecting subset for all elements:\n%s" *)
                       (*   (to_string t) *)
                       (* ; *)

                       (* build non-intersecting subset to add *)
                       let diff_subset_to_add =
                         Bin_int_set.diff
                           bin_int_set_to_add_remaining
                           inter_subset
                       in

                       (* return non-intersecting subset for further intersection/processing *)
                       diff_subset_to_add            
                     )
                 )
            )
            ~init: bin_int_set_to_add
        in

        (* if Bin_int_set.cardinal bin_int_set_remaining_after_adding > 0 then *)
        if Bin_int_set.is_empty bin_int_set_remaining_after_adding = false then
          (
            add_subset
              t
              (Core_kernel.Core_int.Set.singleton element_indice)
              bin_int_set_remaining_after_adding;
          );

        debug "add: end";
      )

    let remove_element_indice
        t
        element_indice
      =
      (
        let subset_indice_set =
          Int_core_ht.find_exn
            t.element_subset_set_h
            element_indice
        in

        Core_kernel.Core_int.Set.iter
          subset_indice_set
          (fun subset_indice ->
             let element_indice_set_found =
               Int_core_ht.find_exn
                 t.subset_element_set_h
                 subset_indice
             in

             assert(Core_kernel.Core_int.Set.length element_indice_set_found > 0);

             if Core_kernel.Core_int.Set.length element_indice_set_found = 1 then
               (
                 (* current subset is only reference by element to remove *)
                 Int_core_ht.remove
                   t.subset_element_set_h
                   subset_indice;

                 Int_core_ht.remove
                   t.subset_h
                   subset_indice;
               )
             else
               (
                 let new_element_indice_set =
                   Core_kernel.Core_int.Set.remove
                     element_indice_set_found
                     element_indice
                 in

                 Int_core_ht.replace
                   t.subset_element_set_h
                   subset_indice
                   new_element_indice_set
                 ;
               )      
          )
        ;
      )

    let of_container_compact_list
        container_compact_list
      =
      (
        let new_t = new_empty_t () in

        L.iteri
          (fun element_indice container_compact ->
             let bin_int_l =
               Container_compact.to_list
                 container_compact
             in

             let bin_int_set =
               Bin_int_set.of_list
                 bin_int_l
             in

             add
               new_t
               element_indice
               bin_int_set
          )
          container_compact_list;

        new_t
      )

    let copy t =
      new_t
        t.current_subset_indice
        (Int_core_ht.copy t.subset_h)
        (Int_core_ht.copy t.subset_element_set_h)
        (Int_core_ht.copy t.element_subset_set_h)

    let find_element_indice t element_indice =
      let subset_set =
        Int_core_ht.find_exn
          t.element_subset_set_h
          element_indice
      in

      let subset_indice_list = Core_kernel.Core_int.Set.elements subset_set in

      let subset_list =
        L.map
          (fun subset_indice ->
             Int_core_ht.find_exn
               t.subset_h
               subset_indice
          )
          subset_indice_list
      in

      L.fold_right
        (fun subset subset_acc ->
           if Bin_int_set.subset subset subset_acc = true then
             (
               print_endline
                 (sprintf
                    "\n\nIp_activity_data: unexpected inclusion between sets:\nsubset (%d):\n%s\n\nsubset_acc (%d):\n%s\n\n%s"
                    (Bin_int_set.cardinal subset)
                    (to_string_bin_int_set subset)
                    (Bin_int_set.cardinal subset_acc)
                    (to_string_bin_int_set subset_acc)
                    (to_string t)
                 );
               assert(false);
             );
           Bin_int_set.union
             subset
             subset_acc
        )
        subset_list
        Bin_int_set.empty

    let inter t indice_1 indice_2 =
      let subset_indice_set_1 =
        Int_core_ht.find_exn
          t.element_subset_set_h
          indice_1
      in
      let subset_indice_set_2 =
        Int_core_ht.find_exn
          t.element_subset_set_h
          indice_2
      in

      let subset_indice_set_inter =
        Core_kernel.Core_int.Set.inter
          subset_indice_set_1
          subset_indice_set_2
      in

      match Core_kernel.Core_int.Set.is_empty subset_indice_set_inter with
      | true -> Bin_int_set.empty
      | false ->
        Core_kernel.Core_int.Set.fold
          ~f: (fun subset_acc subset_indice ->
              let subset =
                Int_core_ht.find_exn
                  t.subset_h
                  subset_indice
              in
              Bin_int_set.union subset subset_acc
            )
          subset_indice_set_inter
          ~init: Bin_int_set.empty

    let inter_length t indice_1 indice_2 =
      Bin_int_set.cardinal (inter t indice_1 indice_2)

    let union_length t indice_1 indice_2 =
      let subset_indice_set_1 =
        Int_core_ht.find_exn
          t.element_subset_set_h
          indice_1
      in
      let subset_indice_set_2 =
        Int_core_ht.find_exn
          t.element_subset_set_h
          indice_2
      in

      let subset_indice_set_union =
        Core_kernel.Core_int.Set.union
          subset_indice_set_1
          subset_indice_set_2
      in

      match Core_kernel.Core_int.Set.is_empty subset_indice_set_union with
      | true -> 0
      | false ->
        Core_kernel.Core_int.Set.fold
          ~f: (fun length_acc subset_indice ->
              let subset =
                Int_core_ht.find_exn
                  t.subset_h
                  subset_indice
              in
              length_acc + Bin_int_set.cardinal subset 
            )
          subset_indice_set_union
          ~init: 0    

    let inter_length_global t =
      let subset_indice_set_tuple_list =
        L.map snd (Int_core_ht.to_alist t.element_subset_set_h)
      in

      let subset_indice_set_inter =
        L.fold_right
          (fun subset_indice_set subset_indice_set_inter ->
             Core_kernel.Core_int.Set.inter
               subset_indice_set
               subset_indice_set_inter
          )
          (L.tl subset_indice_set_tuple_list)
          (L.hd subset_indice_set_tuple_list)
      in

      match Core_kernel.Core_int.Set.length subset_indice_set_inter with
      | 0 -> 0
      | 1 -> 
        let subset_indice = Core_kernel.Core_int.Set.min_elt_exn subset_indice_set_inter in
        let subset =
          Int_core_ht.find_exn
            t.subset_h
            subset_indice
        in
        Bin_int_set.cardinal subset
      | _ ->
        print_endline
          (sprintf
             "Ip_activity_data: inter_length_global: several non-overlapping subset in every elements:\n%s"
             (to_string t)
          );
        assert(false)

    let union_length_global t =
      Int_core_ht.fold
        ~f: (fun ~key: subset_indice ~data: subset union_size ->
           union_size + (Bin_int_set.cardinal subset)
        )
        t.subset_h
        ~init: 0

    let union t =
      L.fold_right
        (fun subset union_acc ->
           Bin_int_set.union subset union_acc
        )
        (L.map snd (Int_core_ht.to_alist t.subset_h))
        Bin_int_set.empty

    let relative_inter_length t element_indice =
      let subset_indice_set =
        Int_core_ht.find_exn
          t.element_subset_set_h
          element_indice
      in

      Core_kernel.Core_int.Set.fold
        ~f: (fun length_acc subset_indice ->
            let element_indice_set_found =
              Int_core_ht.find_exn
                t.subset_element_set_h
                subset_indice
            in

            match Core_kernel.Core_int.Set.length element_indice_set_found with
            | 0 -> failwith "Empty element_indice_set found !!!!"
            | 1 -> 
              (* this subset belong to this element indice only *)
              length_acc
            | _ ->
              (* this subset belong to this element indice and others *)
              let subset_found =
                Int_core_ht.find_exn
                  t.subset_h
                  subset_indice
              in

              length_acc + (Bin_int_set.cardinal subset_found)
          )
        subset_indice_set
        ~init: 0

    let fold f t acc =
      Int_core_ht.fold
        ~f: (fun ~key: (element_indice : int) ~data: subset_indice_set h_acc ->
           Core_kernel.Core_int.Set.fold
             ~f: (fun s_acc (subset_indice : int) ->
                 let subset =
                   Int_core_ht.find_exn
                     t.subset_h
                     subset_indice
                 in

                 f
                   element_indice
                   subset
                   s_acc
               )
             subset_indice_set
             ~init: h_acc
        )
        t.element_subset_set_h
        ~init: acc

    let fold_container f t acc =
      Int_core_ht.fold
        ~f: (fun ~key: (element_indice : int) ~data: subset_indice_set h_acc ->
           Core_kernel.Core_int.Set.fold
             ~f: (fun s_acc (subset_indice : int) ->
                 let subset =
                   Int_core_ht.find_exn
                     t.subset_h
                     subset_indice
                 in

                 let container =
                   Container.of_list
                     (L.map
                        Unsigned_int.of_bin_int
                        (Bin_int_set.to_list subset)
                     )
                 in

                 f
                   element_indice
                   container
                   s_acc
               )
             subset_indice_set
             ~init: h_acc
        )
        t.element_subset_set_h
        ~init: acc

  end  

end
