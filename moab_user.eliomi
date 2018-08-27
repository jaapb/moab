[%%server.start]

val verify_password: string -> string -> int64 Lwt.t

[%%shared.start]

val connected_user_box: Os_types.User.t -> [> `Div] Eliom_content.Html.D.elt
val user_box: Os_types.User.t option -> [> `Div] Eliom_content.Html.D.elt Lwt.t

type user_type = Admin | Examiner | Student

val get_user_type: int64 -> user_type Lwt.t

val add_student: string * string * string -> unit Lwt.t
