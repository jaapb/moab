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
]

let no_session_found () =
	container
	[
		h1 [pcdata "No session found"];
		p [pcdata "No session was found at this time. This action will be logged."]
	]
;;

let attendance_page () () =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get user in
		match u with
		| None -> container []
		| Some _ -> 
			let%lwt session = Moab_db.find_sessions_now () in
			match session with
			| `Lecture | `Test ->
				container
				[
					h1 [pcdata "Lecture"];
					p [pcdata "You can register your attendance."]
				]
			| `Seminar ->
				container
				[
					h1 [pcdata "Seminar"];
					p [pcdata "You can register your attendance. If there are presentations scheduled in this session, then submitting one or more feedback forms will automatically register you for attendance as well."]
				]
	)
	(function
	| Not_found -> no_session_found ()
	| Invalid_argument s -> 
		container
		[
			h1 [pcdata "Error"];
			p [pcdata (Printf.sprintf  "There was an inconsistency in the database (message: %s)." s)]
		]
	| e -> error_page (Printexc.to_string e))
;;

let () =
  Moab_app.register ~service:attendance_service attendance_page;
;;
