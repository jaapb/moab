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

let no_session_found () =
	container
	[
		h1 [pcdata "No session found"];
		p [pcdata "No session was found at this time. This action will be logged."];
		p [a ~service:main_service [pcdata "Return to the main menu"] ()]
	]
;;

let do_attendance_page () (user_id, session_id) =
	Lwt.catch (fun () ->
		let week = Date.week (Date.today ()) in
		let%lwt () = Moab_db.register_attendance session_id user_id week in
		container
		[
			h1 [pcdata "Attendance"];
			p [pcdata "Your attendance has been successfully registered for this session."];
			p [a ~service:main_service [pcdata "Return to main menu"] ()]
		]
	)
	(function
	| Not_found -> error_page "User or session does not exist"
	| e -> error_page (Printexc.to_string e)
	)
;;

let attendance_page () () =
	let do_attendance_service = create_attached_post ~fallback:attendance_service
		~post_params:(string "user_id" ** int32 "session_id") () in
	let register_attendance_form uid sid =
	begin
		Form.post_form ~service:do_attendance_service
		(fun (user_id, session_id) ->
		[
			Form.input ~input_type:`Hidden ~name:user_id ~value:uid Form.string;
			Form.input ~input_type:`Hidden ~name:session_id ~value:sid Form.int32;
			Form.input ~input_type:`Submit ~value:"Register" Form.string
		]
		) ()
	end in
	Moab_app.register ~scope:Eliom_common.default_session_scope
		~service:do_attendance_service do_attendance_page;
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get user in
		match u with
		| None -> container []
		| Some (uid, _, _) -> 
			let%lwt (session_id, session_type) = Moab_db.find_sessions_now () in
			match session_type with
			| `Lecture | `Test ->
				container
				(	
					h1 [pcdata "Lecture"]::
					[register_attendance_form uid session_id]
				)
			| `Seminar ->
				container
				[
					h1 [pcdata "Seminar"];
					p [pcdata "You can register your attendance. If there are presentations scheduled in this session, then submitting one or more feedback forms will automatically register you for attendance as well."]
				]
	)
	(function
	| Not_found -> no_session_found ()
	| Failure s -> 
		container
		[
			h1 [pcdata "Error"];
			p [pcdata (Printf.sprintf "There was an inconsistency in the database (message: %s)." s)]
		]
	| e -> error_page (Printexc.to_string e))
;;

let () =
  Moab_app.register ~service:attendance_service attendance_page;
;;
