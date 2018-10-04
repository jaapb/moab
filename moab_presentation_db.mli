val get_schedule: string -> int -> (int32 * int64 option * int64 option) list Lwt.t
val schedule_presentation: string -> int -> bool -> int64 -> unit Lwt.t
