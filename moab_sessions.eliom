[%%shared
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

let%shared real_setup_sessions_handler myid () () =
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]] [
			h1 [pcdata [%i18n S.setup_sessions ~capitalize:true]]
		]
	]

let%server setup_sessions_handler myid () () =
	(*Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:setup_terms_action do_setup_terms; *)
	real_setup_sessions_handler myid () ()

let%client setup_sessions_handler =
	real_setup_sessions_handler
