val get_academic_years: unit -> string list Lwt.t
val add_term: string -> int -> int -> int -> unit Lwt.t
val get_learning_weeks: string -> (int64 * int32 * int) list Lwt.t
val get_term_ids: string -> int64 list Lwt.t
val get_current_term: string -> int64 Lwt.t
