[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

(* Database access *)

let%server get_attendance sid =
	Moab_attendance_db.get_attendance sid

let%client get_attendance =
	~%(Eliom_client.server_function [%derive.json : int64]
		(Os_session.connected_wrapper get_attendance))

(* Handlers *)

let%server do_register_attendance () () =
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared register_attendance_handler myid () () =
	let%lwt sids = Moab_sessions.get_current_sessions "2018-19" in
	match sids with
	| [] -> Moab_container.page (Some myid) [p [pcdata [%i18n S.no_session_now]]]
 	| sid::_ ->
		let%lwt attendance = get_attendance sid in
		let (attendance_l, attendance_h) = Eliom_shared.ReactiveData.RList.create (List.map (fun (uid, mid, fn, ln) -> (Some (uid, fn, ln), mid)) attendance) in 
		let attendance_rows l = Eliom_shared.ReactiveData.RList.map 
			[%shared ((fun (user, mdx_id) ->
				match user with
				| None -> tr ~a:[a_class ["unknown-user"]] [
						td [pcdata mdx_id];
						td []
					]
				| Some (uid, fn, ln) -> tr [
						td [pcdata mdx_id];
						td [pcdata fn; pcdata " "; pcdata ln]
					]
			): _ -> _)] l in
		let student_id_input = D.Raw.input () in
		ignore [%client ((Lwt.async @@ fun () ->
			let student_id_regexp = Re.Str.regexp "M[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]" in
			let inp = Eliom_content.Html.To_dom.of_element ~%student_id_input in
			Lwt_js_events.changes inp @@ fun _ _ ->
			Js.Opt.case (Dom_html.CoerceTo.input inp)
				(fun () -> Lwt.return_unit)
				(fun s -> let student_id = (Js.to_string s##.value) in
					if Re.Str.string_match student_id_regexp student_id 0 = true
					then let%lwt x = Moab_students.find_student_opt student_id in
					begin
						match x with
						| None ->
								Eliom_shared.ReactiveData.RList.snoc (None, String.uppercase_ascii student_id) ~%attendance_h;
								Lwt.return_unit
						|	Some uid -> 
							let%lwt (fn, ln) = Moab_users.get_name uid in
							Eliom_shared.ReactiveData.RList.snoc (Some (uid, fn, ln), String.uppercase_ascii student_id) ~%attendance_h;
							Lwt.return_unit
					end
					else
					begin
						Os_msg.msg ~level:`Err [%i18n S.invalid_student_id];
						Lwt.return_unit
					end
				)
		): unit)];
		Moab_container.page (Some myid)
		[
			div ~a:[a_class ["content-box"]] [
				h1 [pcdata [%i18n S.register_attendance]];
				table [
					tr [
						th [pcdata [%i18n S.student_id]];
						td [student_id_input];
					]
				];
				R.table (attendance_rows attendance_l)
			]
		] 
