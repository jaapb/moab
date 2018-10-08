val set_student_info: int64 -> string -> string -> int -> unit Lwt.t
val get_group_number: string -> int64 -> int option Lwt.t
val set_group_number: string -> int64 -> int option -> unit Lwt.t
val get_group_numbers: string -> int list Lwt.t
val find_student: string -> int64 Lwt.t
val get_students: string -> int option -> int64 list Lwt.t
val get_student_id: int64 -> string Lwt.t
