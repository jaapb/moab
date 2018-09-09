[%%shared
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

let%shared real_edit_blog_handler myid () () =
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]] [
			h1 [pcdata "Blog"]
		]
	]

let%server edit_blog_handler myid () () =
	(*Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:setup_terms_action do_setup_terms; *)
	real_edit_blog_handler myid () ()

let%client edit_blog_handler =
	real_edit_blog_handler
