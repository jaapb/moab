[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

(* Database access *)

let%server get_attendance (sid, lw) =
	Moab_attendance_db.get_attendance sid lw

let%client get_attendance =
	~%(Eliom_client.server_function [%derive.json : int64 * int]
		(Os_session.connected_wrapper get_attendance))

let%server add_attendance (sid, uid, lw) =
	Moab_attendance_db.add_attendance sid uid lw

let%client add_attendance =
	~%(Eliom_client.server_function [%derive.json : int64 * int64 * int]
		(Os_session.connected_wrapper add_attendance))

let%server get_week_attendance (uid, ayear, year, lw) =
	Moab_attendance_db.get_week_attendance uid ayear year lw

let%client get_week_attendance =
	~%(Eliom_client.server_function [%derive.json : int64 * string * int * int]
		(Os_session.connected_wrapper get_week_attendance))

(* Utility functions *)

let%shared attendance_table uid =
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt weeks = Moab_terms.get_learning_weeks ayear in
	let year = Date.year (Date.today ()) in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in 
	let%lwt week_list = Lwt_list.mapi_s (fun i (w, y) ->
		let week_nr = i + 1 in
		let%lwt (a, s) = get_week_attendance (uid, ayear, year, week_nr) in
		let att_class = 
			match lw with
			| None -> []
			| Some learning_week ->
				if week_nr > learning_week then []
				else if a = s then ["full-attendance"]
				else if a = 0 then ["no-attendance"]
				else ["some-attendance"] in
		Lwt.return @@	td ~a:[a_class att_class] [pcdata (string_of_int week_nr)]
	) weeks in
	Lwt.return @@ table ~a:[a_class ["attendance-table"]] [
		tr (
			td [pcdata [%i18n S.your_attendance]]::
			week_list
		)
	]

(* Handlers *)

let%server do_register_attendance () () =
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared register_attendance_handler myid () () =
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt sids = Moab_sessions.get_current_sessions ayear in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let learning_week = match lw with
		| None -> 0
		| Some x -> x in
	match sids with
	| [] -> Moab_container.page (Some myid) [p [pcdata [%i18n S.no_session_now]]]
 	| sid::_ ->
		let%lwt attendance = get_attendance (sid, learning_week) in
		let (attendance_l, attendance_h) = Eliom_shared.ReactiveData.RList.create attendance in
		let attendance_rows l = Eliom_shared.ReactiveData.RList.map 
			[%shared ((fun (uid, mdx_id, fn, ln) ->
				tr [
					td [Moab_icons.D.trash ()];
					td [pcdata mdx_id];
					td [pcdata fn; pcdata " "; pcdata ln]
				]
			): _ -> _)] l in
		let student_id_input = D.Raw.input () in
		ignore [%client ((Lwt.async @@ fun () ->
			let inp = Eliom_content.Html.To_dom.of_element ~%student_id_input in
			Lwt_js_events.changes inp @@ fun _ _ ->
			Js.Opt.case (Dom_html.CoerceTo.input inp)
				(fun () -> Lwt.return_unit)
				(fun s -> let student_id = String.uppercase_ascii (Js.to_string s##.value) in
					let%lwt x = Moab_students.find_student_opt student_id in
					let%lwt () = match x with
					| None -> 
							Os_msg.msg ~level:`Err [%i18n S.unknown_student];
							Lwt.return_unit
					|	Some uid ->
							let%lwt (fn, ln) = Moab_users.get_name uid in
							let%lwt () = add_attendance (~%sid, uid, ~%learning_week) in
							Eliom_shared.ReactiveData.RList.snoc (uid, student_id, fn, ln) ~%attendance_h;
							Lwt.return_unit in
					s##.value := Js.string "";
					s##focus;
					Lwt.return_unit
				)
		): unit)];
		let focus_f = [%client (fun _ -> 
			let inp = Eliom_content.Html.To_dom.of_element ~%student_id_input in
			Js.Opt.case (Dom_html.CoerceTo.input inp)
			(fun () -> ())
			(fun s -> s##focus)
		)] in
		Moab_container.page ~a:[a_onload focus_f] (Some myid)
		[
			div ~a:[a_class ["content-box"]] [
				h1 [pcdata [%i18n S.register_attendance]];
				table [
					tr [
						th [pcdata [%i18n S.student_id]];
						td [student_id_input];
					]
				];
				R.table ~thead:(Eliom_shared.React.S.const (thead [
					tr [
						th [];
						th [pcdata [%i18n S.student_id]];
						th [pcdata [%i18n S.name]]
					]
				]))
				(attendance_rows attendance_l)
			]
		] 
