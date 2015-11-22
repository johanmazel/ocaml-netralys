
open Bin_prot.Std


module Bin_int_V4 = struct
  type t = Core.Std.Int32.t
  with compare, sexp, bin_io

  let of_int32 int32 = int32
  let of_int64_tuple _ = raise (Invalid_argument "Cannot build V4 from two int64")
  let to_int = Int32.to_int

  let min = Int32.min_int
  let max = Int32.max_int

  let succ  = Int32.succ
  let pred  = Int32.pred
  let sub = Int32.sub
end

module Unsigned_int_V4 = struct
  module Bin_int = Bin_int_V4
  
  type t = Uint32.t
  with compare

  (* let compare_unsigned_int = Uint32.compare *)

  let min = Uint32.min_int
  let max = Uint32.max_int

  let of_bin_int = Uint32.of_int32
  let to_bin_int = Uint32.to_int32
  let to_int = Uint32.to_int

  let succ = Uint32.succ
  let pred = Uint32.pred
  let sub = Uint32.sub
end

module Ip_address_V4 = struct
  module Bin_int = Bin_int_V4
  module Unsigned_int = Unsigned_int_V4
    
  type t = Ipaddr.V4.t
  with compare
    
  let null = Ipaddr.V4.of_int32 (Int32.zero)
  let of_bin_int int32 = Ipaddr.V4.of_int32 int32
  let of_unsigned_int uint32 = Ipaddr.V4.of_int32 (Uint32.to_int32 uint32)
  let to_string = Ipaddr.V4.to_string
  let to_ipaddr t = Ipaddr.V4 t      
end
  
module Prefix_V4 = struct
  module Ip_address = Ip_address_V4
    
  type t = Ipaddr.V4.Prefix.t
    
  let to_string prefix = Ipaddr.V4.Prefix.to_string prefix
  let make = Ipaddr.V4.Prefix.make 
  let compare = Ipaddr.V4.Prefix.compare
  let bits = Ipaddr.V4.Prefix.bits
  let network_address = Ipaddr.V4.Prefix.network_address
  let network = Ipaddr.V4.Prefix.network
  let broadcast = Ipaddr.V4.Prefix.broadcast
  let mem = Ipaddr.V4.Prefix.mem
  let max_size = 32
end

module Ip_address_data_structures_V4 = Ip_address_data_structures.Make(Bin_int_V4)(Unsigned_int_V4)(Ip_address_V4)(Prefix_V4)




module Bin_int_V6 = struct
  type t = Core.Std.Int64.t * Core.Std.Int64.t
  with compare, sexp, bin_io

  let zero = Int64.zero, Int64.zero
  let one = Int64.zero, Int64.zero

  let of_int32 int32 = raise (Invalid_argument "Cannot build v6 from int32")
  let of_int64_tuple a b =
    (a, b)
    
  let to_int (a ,b) =
    if a <> Int64.zero then
      raise (Invalid_argument "V6: bin_to_int: losing information");
    Int64.to_int b

  let min = (Int64.min_int, Int64.min_int)
  let max = (Int64.max_int, Int64.max_int)

  let add (a1, b1) (a2, b2) =
    let int64_1_hi, int64_1_lo = (a1, b1) in
    let int64_2_hi, int64_2_lo = (a2, b2) in

    let l = Int64.add int64_1_lo int64_2_lo in
    let h = Int64.add int64_1_hi int64_2_hi in
    if l < int64_1_lo then
      of_int64_tuple
        (Int64.succ h)
        l      
    else
      of_int64_tuple
        h
        l      
                 
  let sub (a1, b1) (a2, b2) =
    let int64_1_hi, int64_1_lo = (a1, b1) in
    let int64_2_hi, int64_2_lo = (a2, b2) in

    let l = Int64.sub int64_1_lo int64_2_lo in
    let h = Int64.sub int64_1_hi int64_2_hi in
    if int64_1_lo < int64_2_lo then
      of_int64_tuple
        (Int64.pred h)
        l
    else
      of_int64_tuple
        h
        l
    
  let succ t = add t one
  let pred t = sub t one
end

module Unsigned_int_V6 = struct
  module Bin_int = Bin_int_V6

  type t = Uint128_custom.t
  with compare

  (* let compare_unsigned_int = Uint128_custom.compare *)
  let compare = Uint128_custom.compare

  let min = Uint128_custom.min_int
  let max = Uint128_custom.max_int

  let of_bin_int bin_int =
    let a, b = bin_int in
    Uint128_custom.of_int64s a b
  let to_bin_int = Uint128_custom.to_int64s
  let to_int = Uint128_custom.to_int

  let succ = Uint128_custom.succ
  let pred = Uint128_custom.pred
  let sub = Uint128_custom.sub
end
  
module Ip_address_V6 = struct
  module Bin_int = Bin_int_V6
  module Unsigned_int = Unsigned_int_V6
    
  type t = Ipaddr.V6.t
  with compare
    
  let null = Ipaddr.V6.of_int64 (Int64.zero, Int64.zero)
  let of_bin_int
      bin_int
    =
    Ipaddr.V6.of_int32
      (Uint128_custom.to_int32s
         (Unsigned_int.of_bin_int
            bin_int
         )
      )      
  let of_unsigned_int uint128 =
    Ipaddr.V6.of_int32 (Uint128_custom.to_int32s uint128)
  let to_string ip_address = Ipaddr.V6.to_string ~v4: false ip_address
  let to_ipaddr t = Ipaddr.V6 t
end

module Prefix_V6 = struct
  module Ip_address = Ip_address_V6
    
  type t = Ipaddr.V6.Prefix.t
    
  let to_string prefix = Ipaddr.V6.Prefix.to_string prefix
  let make = Ipaddr.V6.Prefix.make 
  let compare = Ipaddr.V6.Prefix.compare
  let bits = Ipaddr.V6.Prefix.bits
  let network_address = Ipaddr.V6.Prefix.network_address
  let network = Ipaddr.V6.Prefix.network
  let broadcast _ = Ipaddr.V6.of_int32 (Int32.max_int, Int32.max_int, Int32.max_int, Int32.max_int)
  let mem = Ipaddr.V6.Prefix.mem
  let max_size = 128
end

module Ip_address_data_structures_V6 = Ip_address_data_structures.Make(Bin_int_V6)(Unsigned_int_V6)(Ip_address_V6)(Prefix_V6)
