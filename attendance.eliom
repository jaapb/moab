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

let is_internal_ip str =
	match !Moab.inside_res with
	| [] -> true
	| l -> List.fold_left (fun acc re ->
		acc || Re_str.string_match re str 0
	) false l
;;

let attendance_page () () =
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, _, _) -> 
		Lwt.catch (fun () ->
			let remote_ip = Eliom_request_info.get_remote_ip () in
			let%lwt (session_id, session_type, sgroup) = Moab_db.find_sessions_now () in
			let%lwt (group, _, _) = Moab_db.get_user_group uid !Moab.term in
			let%lwt clw = Moab_db.current_learning_week group !Moab.term in
			match clw with
			| None -> container 
				[
					h1 [pcdata "Free"];
					p [pcdata "We are not in term, there is no need to register attendance."]
				]
			| Some lw -> let%lwt att = Moab_db.has_attended session_id uid lw in
				(if not (is_internal_ip remote_ip)
				then
					Moab_db.log uid remote_ip `External_address >>=
					fun () -> container
					[
						h1 [pcdata "Exterior address"];
						p [pcdata "You tried to register from a non-Middlesex IP address. This
						action will be logged."]
					]
				else if att then
					container
					[
						h1 [pcdata "Already registered"];
						p [pcdata "Your attendance has already been registered for this session."]
					]
				else match session_type with
				| `No_session -> no_session_found uid
				| `Lecture | `Test ->
					Moab_db.register_attendance session_id uid lw >>=
					fun () -> container [
						h1 [pcdata "Attendance"];
						p [pcdata "Your attendance has been successfully registered for this session."]
					]
				| `Seminar ->
					Moab_db.register_attendance session_id uid lw >>=
					fun () -> container (
						h1 [pcdata "Attendance"]::
						p [pcdata "Your attendance has been successfully registered for this session."]::
						if Some group <> sgroup then
							[p [pcdata "However, since you are not normally part of this group, it will have to be confirmed. Please see your tutor after the session."]]
						else
							[]
					)
				)
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
  Eliom_registration.Any.register ~service:attendance_service attendance_page;
;;
