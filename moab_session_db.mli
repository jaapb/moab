open CalendarLib

val get_sessions: string -> (int64 * int64 * string * int * Time.t * Time.t * string option) list Lwt.t
val get_fresh_session_id: unit -> int64 Lwt.t
