val get_blog: int64 -> string -> int -> (string * string) Lwt.t
val update_blog: int64 -> string -> int -> string -> string -> unit Lwt.t
val get_nr_blogs: int64 -> string -> bool -> int64 Lwt.t
val get_approvable_blogs: string -> (int64 * string * int) list Lwt.t
