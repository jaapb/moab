[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
]

(* Local services *)

(* let%server setup_sessions_action = Eliom_service.create_attached_post
	~name:"setup_sessions_action"
	~fallback:Moab_services.setup_sessions_service
	~post_params:(any)
	()

let%client setup_sessions_action = 
	~%setup_sessions_action *)

(* Database access *)

(* Utility functions *)

(* Handlers *)
	
let%shared submit_report_handler myid () () =
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]] [
			h1 [pcdata [%i18n S.submit_report]]
		]
	]
