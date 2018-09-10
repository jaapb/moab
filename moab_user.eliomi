[%%shared.start]

val find_user: string -> int64 Lwt.t

type user_type = Admin | Examiner | Student

val get_user_type: int64 -> user_type Lwt.t

val add_user: user_type * string * string * string * string option -> int64 Lwt.t

val connected_user_box: Os_types.User.t -> [> `Div] Eliom_content.Html.D.elt
val user_box: Os_types.User.t option -> [> `Div] Eliom_content.Html.D.elt Lwt.t

[%%server.start]

val verify_password: string -> string -> int64 Lwt.t

