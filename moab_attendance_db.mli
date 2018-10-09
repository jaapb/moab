val get_session_attendance: int64 -> int -> (int64 * string * string * string) list Lwt.t
val add_session_attendance: int64 -> int64 -> int -> unit Lwt.t
val get_week_attendance: int64 -> string -> int64 -> int -> (int * int) Lwt.t
val get_attendance_list: string -> int -> (int64 * int64 * int64) list Lwt.t
