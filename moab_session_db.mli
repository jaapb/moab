open CalendarLib

val get_sessions: string -> (int64 * int64 * string * int * Time.t * Time.t * string option * int option) list Lwt.t
val get_fresh_session_id: unit -> int64 Lwt.t
val add_session: int64 option -> string -> int64 -> string -> int -> string -> string -> string option -> int option -> unit Lwt.t
val get_current_sessions: string -> int64 list Lwt.t
val get_week_sessions: string -> int -> int -> int64 list Lwt.t
val find_sessions: string -> string -> int option -> int64 list Lwt.t
val get_session_weekday: int64 -> int Lwt.t
val get_session_time: int64 -> (Time.t * Time.t) Lwt.t
val get_session_room: int64 -> string option Lwt.t
