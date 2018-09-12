open CalendarLib

val get_sessions: string -> (int64 * int64 * string * int * Time.t * Time.t * string option) list Lwt.t
