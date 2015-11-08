
type t =
  {
    tv_sec: int;
    tv_usec: int;
  }

let new_t
    tv_sec
    tv_usec
  =
  {
    tv_sec;
    tv_usec;
  }

let copy t =
  new_t
    t.tv_sec
    t.tv_usec

let get_capture_time_of_packet_in_float
    t
  =
  let time_sec = (float_of_int t.tv_sec) in
  let time_usec = (float_of_int t.tv_usec) in
  let result = time_sec +. (time_usec *. 0.000001) in
  result
