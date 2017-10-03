[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
]

[%%server
	open Services
	open Moab
	open CalendarLib
]

let no_session_found uid =
	Moab_db.log uid (Eliom_request_info.get_remote_ip ()) `No_session_found >>=
	fun () -> container [
		h1 [pcdata "No session found"];
		p [pcdata "No session was found at this time. This action has been logged."]
	]
;;

let do_feedback_page () first =
	Eliom_registration.Redirection.send (Eliom_registration.Redirection main_service)
	;;

let feedback_page () () =
	let do_feedback_service = create_attached_post ~fallback:feedback_service
		~post_params:(radio string "presenter") () in
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:do_feedback_service do_feedback_page;
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, _, _) -> 
		Lwt.catch (fun () ->
			let week = Date.week (Date.today ()) in
			let%lwt (session_id, session_type) = Moab_db.find_sessions_now () in
			let%lwt (group, _, _) = Moab_db.get_user_group uid !Moab.term in
			let%lwt this_lw = Moab_db.current_learning_week group !Moab.term in
			let%lwt (p1, p2) = match this_lw with
			| None -> Lwt.return (None, None)
			| Some lw -> Moab_db.get_presenters !term group lw in
			match session_type with
			| `No_session -> no_session_found uid
			| `Lecture | `Test ->
				container
				[	
					h1 [pcdata "Lecture"];
					p [pcdata "The session currently running is a lecture, no feedback required."]
				]
			| `Seminar -> 
				container
				[
					h1 [pcdata "Feedback"];
					Form.post_form ~service:do_feedback_service (fun (presenter_id) -> [
						table [
							tr [
								th [pcdata "Presenter: "];
								td (match p1, p2 with
								| None, None ->
									[i [pcdata "no presentations scheduled"]]
								| Some (i1, n1), None when i1 = uid ->
									[i [pcdata "Yourself"]]
								| None, Some (i2, n2) when i2 = uid ->
									[i [pcdata "Yourself"]]
								| Some (i1, n1), None ->
									[Form.radio ~checked:true ~name:presenter_id ~value:i1 Form.string; pcdata " "; pcdata n1]
								| Some (i1, n1), Some (i2, n2) when i2 = uid ->
									[Form.radio ~checked:true ~name:presenter_id ~value:i1 Form.string; pcdata " "; pcdata n1]
								| None, Some (i2, n2) ->
									[Form.radio ~checked:true ~name:presenter_id ~value:i2 Form.string; pcdata " "; pcdata n2]
								| Some (i1, n1), Some (i2, n2) when i1 = uid ->
									[Form.radio ~checked:true ~name:presenter_id ~value:i2 Form.string; pcdata " "; pcdata n2]
								| Some (i1, n1), Some (i2, n2) ->
									[Form.radio ~checked:true ~name:presenter_id ~value:i1 Form.string; pcdata " "; pcdata n1;
									pcdata " ";
									Form.radio ~name:presenter_id ~value:i2 Form.string; pcdata " "; pcdata n2]
								)
							]
						]
					]) ()
				]
		)
		(function
		| Failure s -> 
			container
			[
				h1 [pcdata "Error"];
				p [pcdata (Printf.sprintf "There was an inconsistency in the database (message: %s)." s)]
			]
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Eliom_registration.Any.register ~service:feedback_service feedback_page;
;;
