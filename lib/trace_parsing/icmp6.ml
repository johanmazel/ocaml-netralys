(* This file was autogenerated by MPL *)
module Icmp6 = struct
  open Mpl_stdlib
  exception Bad_packet of string

  module EchoReply = struct
    class o
      ~(data_length:int)
      (env:env) =
      object(self)
        method env = env_at env 0 self#sizeof
        method sizeof = data_length+2+2+2+1+1

        method code =
          Mpl_byte.to_int (Mpl_byte.at env (1))
        method set_code v : unit =
          Mpl_byte.marshal (env_at env (1) 1) (Mpl_byte.of_int v)

        method checksum =
          Mpl_uint16.to_int (Mpl_uint16.at env (1+1))
        method set_checksum v : unit =
          Mpl_uint16.marshal (env_at env (1+1) 2) (Mpl_uint16.of_int v)

        method identifier =
          Mpl_uint16.to_int (Mpl_uint16.at env (1+1+2))
        method set_identifier v : unit =
          Mpl_uint16.marshal (env_at env (1+1+2) 2) (Mpl_uint16.of_int v)

        method sequence =
          Mpl_uint16.to_int (Mpl_uint16.at env (1+1+2+2))
        method set_sequence v : unit =
          Mpl_uint16.marshal (env_at env (1+1+2+2) 2) (Mpl_uint16.of_int v)

        method data =
          Mpl_raw.at env (1+1+2+2+2) data_length
        (* set_data unsupported for now (type byte array) *)
        method data_env : env = env_at env (1+1+2+2+2) data_length
        method data_frag = Mpl_raw.frag env (1+1+2+2+2) data_length
        method data_length = data_length


        method prettyprint =
          let out = prerr_endline in
          out "[ Icmp.EchoReply.icmp ]";
          (* ptype : bound *)
          out ("  code = " ^ (Printf.sprintf "%u" self#code));
          out ("  checksum = " ^ (Printf.sprintf "%u" self#checksum));
          out ("  identifier = " ^ (Printf.sprintf "%u" self#identifier));
          out ("  sequence = " ^ (Printf.sprintf "%u" self#sequence));
          out ("  data = " ^ (Mpl_raw.prettyprint self#data));
          ()
      end

    let t
      ?(code=0)
      ?(checksum=0)
      ~identifier
      ~sequence
      ~(data:data)
      env =
        let ___env = env_at env (1+1+2+2+2) 0 in
        let data___len = match data with 
        |`Str x -> Mpl_raw.marshal ___env x; String.length x
        |`Sub fn -> fn ___env; curpos ___env
        |`None -> 0
        |`Frag t -> Mpl_raw.blit ___env t; curpos ___env in
        let ptype = (Mpl_byte.of_int 129) in (* const *)
        let code = (Mpl_byte.of_int code) in
        let checksum = (Mpl_uint16.of_int checksum) in
        let identifier = (Mpl_uint16.of_int identifier) in
        let sequence = (Mpl_uint16.of_int sequence) in
        (* let data = data in *)
        Mpl_byte.marshal env ptype;
        Mpl_byte.marshal env code;
        Mpl_uint16.marshal env checksum;
        Mpl_uint16.marshal env identifier;
        Mpl_uint16.marshal env sequence;
        skip env data___len;
        new o
        ~data_length:data___len
        env

    let m (x:(env->o)) env = x env
    let sizeof (x:o) = x#sizeof
    let prettyprint (x:o) = x#prettyprint
    let env (x:o) = x#env
  end

  module EchoRequest = struct
    class o
      ~(data_length:int)
      (env:env) =
      object(self)
        method env = env_at env 0 self#sizeof
        method sizeof = data_length+2+2+2+1+1

        method code =
          Mpl_byte.to_int (Mpl_byte.at env (1))
        method set_code v : unit =
          Mpl_byte.marshal (env_at env (1) 1) (Mpl_byte.of_int v)

        method checksum =
          Mpl_uint16.to_int (Mpl_uint16.at env (1+1))
        method set_checksum v : unit =
          Mpl_uint16.marshal (env_at env (1+1) 2) (Mpl_uint16.of_int v)

        method identifier =
          Mpl_uint16.to_int (Mpl_uint16.at env (1+1+2))
        method set_identifier v : unit =
          Mpl_uint16.marshal (env_at env (1+1+2) 2) (Mpl_uint16.of_int v)

        method sequence =
          Mpl_uint16.to_int (Mpl_uint16.at env (1+1+2+2))
        method set_sequence v : unit =
          Mpl_uint16.marshal (env_at env (1+1+2+2) 2) (Mpl_uint16.of_int v)

        method data =
          Mpl_raw.at env (1+1+2+2+2) data_length
        (* set_data unsupported for now (type byte array) *)
        method data_env : env = env_at env (1+1+2+2+2) data_length
        method data_frag = Mpl_raw.frag env (1+1+2+2+2) data_length
        method data_length = data_length


        method prettyprint =
          let out = prerr_endline in
          out "[ Icmp.EchoRequest.icmp ]";
          (* ptype : bound *)
          out ("  code = " ^ (Printf.sprintf "%u" self#code));
          out ("  checksum = " ^ (Printf.sprintf "%u" self#checksum));
          out ("  identifier = " ^ (Printf.sprintf "%u" self#identifier));
          out ("  sequence = " ^ (Printf.sprintf "%u" self#sequence));
          out ("  data = " ^ (Mpl_raw.prettyprint self#data));
          ()
      end

    let t
      ?(code=0)
      ?(checksum=0)
      ~identifier
      ~sequence
      ~(data:data)
      env =
        let ___env = env_at env (1+1+2+2+2) 0 in
        let data___len = match data with 
        |`Str x -> Mpl_raw.marshal ___env x; String.length x
        |`Sub fn -> fn ___env; curpos ___env
        |`None -> 0
        |`Frag t -> Mpl_raw.blit ___env t; curpos ___env in
        let ptype = (Mpl_byte.of_int 128) in (* const *)
        let code = (Mpl_byte.of_int code) in
        let checksum = (Mpl_uint16.of_int checksum) in
        let identifier = (Mpl_uint16.of_int identifier) in
        let sequence = (Mpl_uint16.of_int sequence) in
        (* let data = data in *)
        Mpl_byte.marshal env ptype;
        Mpl_byte.marshal env code;
        Mpl_uint16.marshal env checksum;
        Mpl_uint16.marshal env identifier;
        Mpl_uint16.marshal env sequence;
        skip env data___len;
        new o
        ~data_length:data___len
        env

    let m (x:(env->o)) env = x env
    let sizeof (x:o) = x#sizeof
    let prettyprint (x:o) = x#prettyprint
    let env (x:o) = x#env
  end

  module ParameterProblem = struct
    class o
      ~(ip6_header_length:int)
      (env:env) =
      object(self)
        method env = env_at env 0 self#sizeof
        method sizeof = ip6_header_length+4+2+1+1

        method code =
          Mpl_byte.to_int (Mpl_byte.at env (1))
        method set_code v : unit =
          Mpl_byte.marshal (env_at env (1) 1) (Mpl_byte.of_int v)

        method checksum =
          Mpl_uint16.to_int (Mpl_uint16.at env (1+1))
        method set_checksum v : unit =
          Mpl_uint16.marshal (env_at env (1+1) 2) (Mpl_uint16.of_int v)


        method ip6_header =
          Mpl_raw.at env (1+1+2+4) ip6_header_length
        (* set_ip6_header unsupported for now (type byte array) *)
        method ip6_header_env : env = env_at env (1+1+2+4) ip6_header_length
        method ip6_header_frag = Mpl_raw.frag env (1+1+2+4) ip6_header_length
        method ip6_header_length = ip6_header_length


        method prettyprint =
          let out = prerr_endline in
          out "[ Icmp.ParameterProblem.icmp ]";
          (* ptype : bound *)
          out ("  code = " ^ (Printf.sprintf "%u" self#code));
          out ("  checksum = " ^ (Printf.sprintf "%u" self#checksum));
          (* reserved : bound *)
          out ("  ip6_header = " ^ (Mpl_raw.prettyprint self#ip6_header));
          ()
      end

    let t
      ?(code=0)
      ?(checksum=0)
      ~(ip6_header:data)
      env =
        let ___env = env_at env (1+1+2+4) 0 in
        let ip6_header___len = match ip6_header with 
        |`Str x -> Mpl_raw.marshal ___env x; String.length x
        |`Sub fn -> fn ___env; curpos ___env
        |`None -> 0
        |`Frag t -> Mpl_raw.blit ___env t; curpos ___env in
        let ptype = (Mpl_byte.of_int 4) in (* const *)
        let reserved = (Mpl_uint32.of_int32 0l) in (* const *)
        let code = (Mpl_byte.of_int code) in
        let checksum = (Mpl_uint16.of_int checksum) in
        (* let ip6_header = ip6_header in *)
        Mpl_byte.marshal env ptype;
        Mpl_byte.marshal env code;
        Mpl_uint16.marshal env checksum;
        Mpl_uint32.marshal env reserved;
        skip env ip6_header___len;
        new o
        ~ip6_header_length:ip6_header___len
        env

    let m (x:(env->o)) env = x env
    let sizeof (x:o) = x#sizeof
    let prettyprint (x:o) = x#prettyprint
    let env (x:o) = x#env
  end

  module TimeExceeded = struct
    class o
      ~(ip6_header_length:int)
      (env:env) =
      object(self)
        method env = env_at env 0 self#sizeof
        method sizeof = ip6_header_length+4+2+1+1

        method code =
          Mpl_byte.to_int (Mpl_byte.at env (1))
        method set_code v : unit =
          Mpl_byte.marshal (env_at env (1) 1) (Mpl_byte.of_int v)

        method checksum =
          Mpl_uint16.to_int (Mpl_uint16.at env (1+1))
        method set_checksum v : unit =
          Mpl_uint16.marshal (env_at env (1+1) 2) (Mpl_uint16.of_int v)


        method ip6_header =
          Mpl_raw.at env (1+1+2+4) ip6_header_length
        (* set_ip6_header unsupported for now (type byte array) *)
        method ip6_header_env : env = env_at env (1+1+2+4) ip6_header_length
        method ip6_header_frag = Mpl_raw.frag env (1+1+2+4) ip6_header_length
        method ip6_header_length = ip6_header_length


        method prettyprint =
          let out = prerr_endline in
          out "[ Icmp.TimeExceeded.icmp ]";
          (* ptype : bound *)
          out ("  code = " ^ (Printf.sprintf "%u" self#code));
          out ("  checksum = " ^ (Printf.sprintf "%u" self#checksum));
          (* reserved : bound *)
          out ("  ip6_header = " ^ (Mpl_raw.prettyprint self#ip6_header));
          ()
      end

    let t
      ?(code=0)
      ?(checksum=0)
      ~(ip6_header:data)
      env =
        let ___env = env_at env (1+1+2+4) 0 in
        let ip6_header___len = match ip6_header with 
        |`Str x -> Mpl_raw.marshal ___env x; String.length x
        |`Sub fn -> fn ___env; curpos ___env
        |`None -> 0
        |`Frag t -> Mpl_raw.blit ___env t; curpos ___env in
        let ptype = (Mpl_byte.of_int 3) in (* const *)
        let reserved = (Mpl_uint32.of_int32 0l) in (* const *)
        let code = (Mpl_byte.of_int code) in
        let checksum = (Mpl_uint16.of_int checksum) in
        (* let ip6_header = ip6_header in *)
        Mpl_byte.marshal env ptype;
        Mpl_byte.marshal env code;
        Mpl_uint16.marshal env checksum;
        Mpl_uint32.marshal env reserved;
        skip env ip6_header___len;
        new o
        ~ip6_header_length:ip6_header___len
        env

    let m (x:(env->o)) env = x env
    let sizeof (x:o) = x#sizeof
    let prettyprint (x:o) = x#prettyprint
    let env (x:o) = x#env
  end

  module PacketTooBig = struct
    class o
      ~(ip6_header_length:int)
      (env:env) =
      object(self)
        method env = env_at env 0 self#sizeof
        method sizeof = ip6_header_length+4+2+1+1

        method code =
          Mpl_byte.to_int (Mpl_byte.at env (1))
        method set_code v : unit =
          Mpl_byte.marshal (env_at env (1) 1) (Mpl_byte.of_int v)

        method checksum =
          Mpl_uint16.to_int (Mpl_uint16.at env (1+1))
        method set_checksum v : unit =
          Mpl_uint16.marshal (env_at env (1+1) 2) (Mpl_uint16.of_int v)


        method ip6_header =
          Mpl_raw.at env (1+1+2+4) ip6_header_length
        (* set_ip6_header unsupported for now (type byte array) *)
        method ip6_header_env : env = env_at env (1+1+2+4) ip6_header_length
        method ip6_header_frag = Mpl_raw.frag env (1+1+2+4) ip6_header_length
        method ip6_header_length = ip6_header_length


        method prettyprint =
          let out = prerr_endline in
          out "[ Icmp.PacketTooBig.icmp ]";
          (* ptype : bound *)
          out ("  code = " ^ (Printf.sprintf "%u" self#code));
          out ("  checksum = " ^ (Printf.sprintf "%u" self#checksum));
          (* mtu : bound *)
          out ("  ip6_header = " ^ (Mpl_raw.prettyprint self#ip6_header));
          ()
      end

    let t
      ?(code=0)
      ?(checksum=0)
      ~(ip6_header:data)
      env =
        let ___env = env_at env (1+1+2+4) 0 in
        let ip6_header___len = match ip6_header with 
        |`Str x -> Mpl_raw.marshal ___env x; String.length x
        |`Sub fn -> fn ___env; curpos ___env
        |`None -> 0
        |`Frag t -> Mpl_raw.blit ___env t; curpos ___env in
        let ptype = (Mpl_byte.of_int 2) in (* const *)
        let mtu = (Mpl_uint32.of_int32 0l) in (* const *)
        let code = (Mpl_byte.of_int code) in
        let checksum = (Mpl_uint16.of_int checksum) in
        (* let ip6_header = ip6_header in *)
        Mpl_byte.marshal env ptype;
        Mpl_byte.marshal env code;
        Mpl_uint16.marshal env checksum;
        Mpl_uint32.marshal env mtu;
        skip env ip6_header___len;
        new o
        ~ip6_header_length:ip6_header___len
        env

    let m (x:(env->o)) env = x env
    let sizeof (x:o) = x#sizeof
    let prettyprint (x:o) = x#prettyprint
    let env (x:o) = x#env
  end

  module DestinationUnreachable = struct
    class o
      ~(ip6_header_length:int)
      (env:env) =
      object(self)
        method env = env_at env 0 self#sizeof
        method sizeof = ip6_header_length+4+2+1+1

        method code =
          Mpl_byte.to_int (Mpl_byte.at env (1))
        method set_code v : unit =
          Mpl_byte.marshal (env_at env (1) 1) (Mpl_byte.of_int v)

        method checksum =
          Mpl_uint16.to_int (Mpl_uint16.at env (1+1))
        method set_checksum v : unit =
          Mpl_uint16.marshal (env_at env (1+1) 2) (Mpl_uint16.of_int v)


        method ip6_header =
          Mpl_raw.at env (1+1+2+4) ip6_header_length
        (* set_ip6_header unsupported for now (type byte array) *)
        method ip6_header_env : env = env_at env (1+1+2+4) ip6_header_length
        method ip6_header_frag = Mpl_raw.frag env (1+1+2+4) ip6_header_length
        method ip6_header_length = ip6_header_length


        method prettyprint =
          let out = prerr_endline in
          out "[ Icmp.DestinationUnreachable.icmp ]";
          (* ptype : bound *)
          out ("  code = " ^ (Printf.sprintf "%u" self#code));
          out ("  checksum = " ^ (Printf.sprintf "%u" self#checksum));
          (* reserved : bound *)
          out ("  ip6_header = " ^ (Mpl_raw.prettyprint self#ip6_header));
          ()
      end

    let t
      ?(code=0)
      ?(checksum=0)
      ~(ip6_header:data)
      env =
        let ___env = env_at env (1+1+2+4) 0 in
        let ip6_header___len = match ip6_header with 
        |`Str x -> Mpl_raw.marshal ___env x; String.length x
        |`Sub fn -> fn ___env; curpos ___env
        |`None -> 0
        |`Frag t -> Mpl_raw.blit ___env t; curpos ___env in
        let ptype = (Mpl_byte.of_int 1) in (* const *)
        let reserved = (Mpl_uint32.of_int32 0l) in (* const *)
        let code = (Mpl_byte.of_int code) in
        let checksum = (Mpl_uint16.of_int checksum) in
        (* let ip6_header = ip6_header in *)
        Mpl_byte.marshal env ptype;
        Mpl_byte.marshal env code;
        Mpl_uint16.marshal env checksum;
        Mpl_uint32.marshal env reserved;
        skip env ip6_header___len;
        new o
        ~ip6_header_length:ip6_header___len
        env

    let m (x:(env->o)) env = x env
    let sizeof (x:o) = x#sizeof
    let prettyprint (x:o) = x#prettyprint
    let env (x:o) = x#env
  end

  type o = [
  |`DestinationUnreachable of DestinationUnreachable.o
  |`PacketTooBig of PacketTooBig.o
  |`TimeExceeded of TimeExceeded.o
  |`ParameterProblem of ParameterProblem.o
  |`EchoRequest of EchoRequest.o
  |`EchoReply of EchoReply.o
  ]

  type x = [
  |`DestinationUnreachable of (env -> DestinationUnreachable.o)
  |`PacketTooBig of (env -> PacketTooBig.o)
  |`TimeExceeded of (env -> TimeExceeded.o)
  |`ParameterProblem of (env -> ParameterProblem.o)
  |`EchoRequest of (env -> EchoRequest.o)
  |`EchoReply of (env -> EchoReply.o)
  ]

  let m (x:x) env : o = match x with
  |`DestinationUnreachable (fn:(env->DestinationUnreachable.o)) -> `DestinationUnreachable (fn env)
  |`PacketTooBig (fn:(env->PacketTooBig.o)) -> `PacketTooBig (fn env)
  |`TimeExceeded (fn:(env->TimeExceeded.o)) -> `TimeExceeded (fn env)
  |`ParameterProblem (fn:(env->ParameterProblem.o)) -> `ParameterProblem (fn env)
  |`EchoRequest (fn:(env->EchoRequest.o)) -> `EchoRequest (fn env)
  |`EchoReply (fn:(env->EchoReply.o)) -> `EchoReply (fn env)

  let prettyprint (x:o) = match x with
  |`DestinationUnreachable x -> x#prettyprint
  |`PacketTooBig x -> x#prettyprint
  |`TimeExceeded x -> x#prettyprint
  |`ParameterProblem x -> x#prettyprint
  |`EchoRequest x -> x#prettyprint
  |`EchoReply x -> x#prettyprint

  let sizeof (x:o) = match x with
  |`DestinationUnreachable x -> x#sizeof
  |`PacketTooBig x -> x#sizeof
  |`TimeExceeded x -> x#sizeof
  |`ParameterProblem x -> x#sizeof
  |`EchoRequest x -> x#sizeof
  |`EchoReply x -> x#sizeof

  let env (x:o) = match x with
  |`DestinationUnreachable x -> x#env
  |`PacketTooBig x -> x#env
  |`TimeExceeded x -> x#env
  |`ParameterProblem x -> x#env
  |`EchoRequest x -> x#env
  |`EchoReply x -> x#env


  let unmarshal 
      (env:env) : o =
    let ptype = Mpl_byte.to_int (Mpl_byte.unmarshal env) in
    skip env 1; (* skipped code *)
    skip env 2; (* skipped checksum *)
    match ptype with
    |129 -> `EchoReply (
        skip env 2; (* skipped identifier *)
        skip env 2; (* skipped sequence *)
        let data_length = (remaining env) in
        skip env data_length; (* skipped data *)
        new EchoReply.o env
          ~data_length:data_length
      )
    |128 -> `EchoRequest (
        skip env 2; (* skipped identifier *)
        skip env 2; (* skipped sequence *)
        let data_length = (remaining env) in
        skip env data_length; (* skipped data *)
        new EchoRequest.o env
          ~data_length:data_length
      )
    |4 -> `ParameterProblem (
        skip env 4; (* skipped reserved *)
        let ip6_header_length = (remaining env) in
        skip env ip6_header_length; (* skipped ip6_header *)
        new ParameterProblem.o env
          ~ip6_header_length:ip6_header_length
      )
    |3 -> `TimeExceeded (
        skip env 4; (* skipped reserved *)
        let ip6_header_length = (remaining env) in
        skip env ip6_header_length; (* skipped ip6_header *)
        new TimeExceeded.o env
          ~ip6_header_length:ip6_header_length
      )
    |2 -> `PacketTooBig (
        skip env 4; (* skipped mtu *)
        let ip6_header_length = (remaining env) in
        skip env ip6_header_length; (* skipped ip6_header *)
        new PacketTooBig.o env
          ~ip6_header_length:ip6_header_length
      )
    |1 -> `DestinationUnreachable (
        skip env 4; (* skipped reserved *)
        let ip6_header_length = (remaining env) in
        skip env ip6_header_length; (* skipped ip6_header *)
        new DestinationUnreachable.o env
          ~ip6_header_length:ip6_header_length
      )
    |x -> raise (Bad_packet (Printf.sprintf ": %d" x))
end

