val update_password: int64 -> string -> unit Lwt.t
val verify_password: string -> string -> int64 Lwt.t

val find_user: string -> int64 Lwt.t

val get_user_type: int64 -> string Lwt.t
val get_name: int64 -> (string * string) Lwt.t

val add_user: string -> string -> string -> string -> string option -> int64 Lwt.t

