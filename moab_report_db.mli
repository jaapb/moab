val set_report_feedback: string -> int64 -> string -> int -> string -> int -> string -> int -> unit Lwt.t 
val get_report_feedback: string -> int64 -> (string * int * string * int * string * int) Lwt.t
