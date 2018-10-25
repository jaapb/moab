val get_schedule: string -> int -> (int32 * int64 option * int64 option) list Lwt.t
val schedule_presentation: string -> int -> int -> bool -> int64 -> unit Lwt.t
val find_presentation: string -> int64 -> (int * bool) Lwt.t
val get_random_unoccupied_student: string -> int -> int -> int64 option Lwt.t
