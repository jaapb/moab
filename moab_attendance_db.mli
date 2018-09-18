val get_attendance: int64 -> int -> (int64 * string * string * string) list Lwt.t
val add_attendance: int64 -> int64 -> int -> unit Lwt.t
