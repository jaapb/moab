val get_blog: int64 -> string -> int -> (string * string) Lwt.t
val update_blog: int64 -> string -> int -> string -> string -> unit Lwt.t
val get_nr_blogs: int64 -> string -> int64 Lwt.t
