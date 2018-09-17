[%%shared
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

(* Local services *)

let%server register_attendance_action = Eliom_service.create_attached_post
	~fallback:Moab_services.register_attendance_service
	~post_params:(unit)
	()

let%client register_attendance_action = 
	~%register_attendance_action

(* Handlers *)

let%server do_register_attendance () () =
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared real_register_attendance_handler myid () () =
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]] [
			h1 [pcdata "Register attendance"]
		]
	]

let%server register_attendance_handler myid () () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:register_attendance_action do_register_attendance;
	real_register_attendance_handler myid () ()

let%client register_attendance_handler =
	real_register_attendance_handler
