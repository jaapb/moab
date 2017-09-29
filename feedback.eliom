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
	fun () -> container (standard_menu ()) [
		h1 [pcdata "No session found"];
		p [pcdata "No session was found at this time. This action has been logged."]
	]
;;

let feedback_page () () =
	(*let do_attendance_service = create_attached_post ~fallback:attendance_service
		~post_params:(string "user_id" ** int32 "session_id") () in
	Moab_app.register ~scope:Eliom_common.default_session_scope
		~service:do_attendance_service do_attendance_page; *)
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> container [] [p [pcdata "Please log in first."]]
	| Some (uid, _, _) -> 
		Lwt.catch (fun () ->
			let week = Date.week (Date.today ()) in
			let%lwt (session_id, session_type) = Moab_db.find_sessions_now () in
			match session_type with
			| `Lecture | `Test ->
				container (standard_menu ())
				[	
					h1 [pcdata "Lecture"];
					p [pcdata "The session currently running is a lecture, no feedback required."]
				]
			| `Seminar -> 
				container (standard_menu ())
				[
					h1 [pcdata "Feedback"];
					p [pcdata "So, yeah."]
				]
		)
		(function
		| Not_found -> no_session_found uid
		| Failure s -> 
			container (standard_menu ())
			[
				h1 [pcdata "Error"];
				p [pcdata (Printf.sprintf "There was an inconsistency in the database (message: %s)." s)]
			]
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Moab_app.register ~service:feedback_service feedback_page;
;;
