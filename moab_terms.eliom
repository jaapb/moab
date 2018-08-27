[%%shared
	open Eliom_content.Html.F
	open Eliom_parameter
]

let%server get_terms () =
	Moab_term_db.get_terms ()

let%client get_terms =
	~%(Eliom_client.server_function [%derive.json : unit]
			(Os_session.connected_wrapper get_terms))

let%server setup_terms_action =
	Eliom_service.create_attached_post
		~fallback:Moab_services.setup_terms_service
		~post_params:(string "old_term" ** string "new_term" ** string "start1" ** string "end1")
		()

let%client setup_terms_action =
	~%setup_terms_action

let%server do_setup_terms () (old_term, (new_term, (s1, e1))) = 
	(* date return: YYYY-MM-DD *)
	Ocsigen_messages.console (fun () -> Printf.sprintf "%s <-> %s" s1 e1);
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared real_setup_terms_handler myid () () =
	let term_opt t =
		Form.Option ([], t, None, false) in
	let%lwt terms = get_terms () in
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]]
		[
			h1 [pcdata "Term setup"];
			Form.post_form ~service:setup_terms_action (fun (old_term, (new_term, (s1, e1))) -> [
				table [
					tr [
						th [pcdata "Select term"];
						th [];
						th [pcdata "add term"]
					];
					tr [
						td [
							Form.select ~name:old_term Form.string
								(Form.Option ([], "", None, true))
								(List.map term_opt terms)
						];
						th [pcdata "OR"];
						td [Form.input ~name:new_term ~input_type:`Text Form.string]
					];
					tr
					[
						td [pcdata "From "; Form.input ~name:s1 ~input_type:`Date Form.string];
						td [];
						td [pcdata "To "; Form.input ~name:e1 ~input_type:`Date Form.string]
					];
					tr
					[ 
						td ~a:[a_colspan 3] [
							Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:"Save" Form.string
						]
					]
				]
			]) ()
		]
	]

let%server setup_terms_handler myid () () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:setup_terms_action do_setup_terms;
	real_setup_terms_handler myid () ()

let%client setup_terms_handler =
	real_setup_terms_handler
