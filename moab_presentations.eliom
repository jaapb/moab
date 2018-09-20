[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

(* Database access *)

(* Utility functions *)

(* Handlers *)

let%server do_schedule_presentation () () =
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared real_schedule_presentation_handler myid () () =
	Moab_container.page [
		div ~a:[a_class ["content-box"]] [
			h1 [pcdata [%i18n S.schedule_presentation]]
		]
	]

let%server schedule_presentation_handler myid () () =
	real_schedule_presentation_handler myid () ()

let%client schedule_presentation_handler =
	real_schedule_presentation_handler
