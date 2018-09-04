val update_password: int64 -> string -> unit Lwt.t
val verify_password: string -> string -> int64 Lwt.t

val get_user_type: int64 -> string Lwt.t

val add_student: string -> string -> int64 Lwt.t