val get_schedule: string -> int -> (int32 * (int64 option * int64 option)) list Lwt.t
val schedule_presentation: string -> int -> int -> bool -> int64 -> unit Lwt.t
val find_presentation: string -> int64 -> (int * bool) Lwt.t
val get_unassigned_students: string -> int -> int -> int64 list Lwt.t
val get_random_unassigned_student: string -> int -> int -> int64 option Lwt.t
val get_criteria: string -> (int64 * string * string option) list Lwt.t
val set_score: int64 -> int64 -> int64 -> int -> string -> unit Lwt.t
