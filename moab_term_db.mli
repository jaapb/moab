val get_academic_years: unit -> string list Lwt.t
val add_term: string -> int -> int -> int -> unit Lwt.t
val get_learning_weeks: string -> (int32 * int) list Lwt.t
