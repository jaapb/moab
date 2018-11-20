[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
	open Lwt
	open Moab_base
]

(* Local services *)

let%server presentation_feedback_action =
	Eliom_service.create_attached_post
		~name:"presentation_feedback_action"
		~fallback:Moab_services.presentation_feedback_service
		~post_params:(radio int64 "presenter_id" ** list "scores" (int64 "criterion_id"))
		()

let%client presentation_feedback_action =
	~%presentation_feedback_action

(* Database access *)

let%server get_schedule (ayear, gnr) =
	Moab_presentation_db.get_schedule ayear gnr

let%client get_schedule =
	~%(Eliom_client.server_function [%derive.json: string * int]
			(Os_session.connected_wrapper get_schedule))

let%server schedule_presentation (ayear, learning_week, gnr, first, userid) =
	Moab_presentation_db.schedule_presentation ayear learning_week gnr first userid

let%client schedule_presentation =
	~%(Eliom_client.server_function [%derive.json: string * int * int * bool * int64]
			(Os_session.connected_wrapper schedule_presentation))

let%server find_presentation (ayear, userid) =
	Moab_presentation_db.find_presentation ayear userid

let%client find_presentation =
	~%(Eliom_client.server_function [%derive.json: string * int64]
			(Os_session.connected_wrapper find_presentation))

let%server find_presentation_opt (ayear, userid) =
	try%lwt
		let%lwt x = find_presentation (ayear, userid) in
		Lwt.return_some x
	with
		Not_found -> Lwt.return_none

let%client find_presentation_opt =
	~%(Eliom_client.server_function [%derive.json: string * int64]
			(Os_session.connected_wrapper find_presentation_opt))

let%server get_random_unassigned_student (ayear, gnr, lw) =
	Moab_presentation_db.get_random_unassigned_student ayear gnr lw

let%client get_random_unassigned_student =
	~%(Eliom_client.server_function [%derive.json: string * int * int]
			(Os_session.connected_wrapper get_random_unassigned_student))

let%server get_unassigned_students (ayear, gnr, lw) =
	Moab_presentation_db.get_unassigned_students ayear gnr lw

let%client get_unassigned_students =
	~%(Eliom_client.server_function [%derive.json: string * int * int]
			(Os_session.connected_wrapper get_unassigned_students))

let%server get_criteria ayear =
	Moab_presentation_db.get_criteria ayear

let%client get_criteria =
	~%(Eliom_client.server_function [%derive.json: string]
			(Os_session.connected_wrapper get_criteria))

(* Utility functions *)

let%shared schedule_table av_clicked myid ayear gnr weekday =
	let sw = ~%(!Moab_config.presentation_start_week) in
	let%lwt schedule = get_schedule (ayear, gnr) >|= drop (sw - 1) in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let%lwt group_members = Moab_students.get_students (ayear, Some gnr, lw) in
	let%lwt learning_weeks = Moab_terms.get_learning_weeks ayear >|= drop (sw - 1) in
	let%lwt trs = map2i_s (fun i (week, (uid1, uid2)) (_, w, y) ->
		let id_string lw f =
			Printf.sprintf "%d%s" lw (if f then "-first" else "-second") in
		let create_field first uid = match uid with
		| None ->
				if (2*i) < List.length group_members then
					Lwt.return @@
					td ~a:[a_class ["available"]; a_onclick av_clicked; a_id (id_string (i+sw) first)]
						[pcdata [%i18n S.available]]
				else
					Lwt.return @@
					td ~a:[a_class ["not-available"]] [pcdata [%i18n S.not_available]]
		| Some u -> let%lwt (fn, ln) = Moab_users.get_name u in
			Lwt.return @@ td [pcdata fn; pcdata " "; pcdata ln] in
		let%lwt f1 = create_field true uid1 in
		let%lwt f2 = create_field false uid2 in
		let (d1, _) = Date.week_first_last (Int32.to_int w) y in
		let d = Date.add d1 (Date.Period.day (weekday - 1)) in
		Lwt.return @@ tr [
			td [
				pcdata (Printer.Date.sprint "%b %d" d);
			];
			f1;
			f2
		]) schedule learning_weeks in
	Lwt.return @@ table ~a:[a_class ["schedule-table"]] (
		tr [
			th [pcdata [%i18n S.date]];
			th [pcdata [%i18n S.first_presenter]];
			th [pcdata [%i18n S.second_presenter]]
		]::
		trs
	)

(* Handlers *)

let%shared do_presentation_feedback () (presenter_id) =
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared schedule_presentation_handler myid () () =
	let ayear = ~%(!Moab_config.current_academic_year) in
	let sc = ~%(!Moab_config.schedule_closed) in
	let%lwt gnr = Moab_students.get_group_number (ayear, myid) in
	let%lwt sids = Moab_sessions.find_sessions (ayear, Seminar, gnr) in
	let%lwt weekday = Moab_sessions.get_session_weekday (List.hd sids) in
	let av_clicked g = [%client fun ev ->
		if ~%sc then
			Os_msg.msg ~level:`Err [%i18n S.schedule_closed]
		else
		Js.Opt.case (ev##.target)
			(fun () -> ())
			(fun e -> Scanf.sscanf (Js.to_string (e##.id)) "%d-%s" (fun lw order ->
				Lwt.async (fun () ->
					let%lwt date = Moab_terms.date_of_learning_week ~%ayear lw (Date.day_of_int ~%weekday) in
					let%lwt ok = Ot_popup.confirm [
							p [
								pcdata [%i18n S.schedule_message1];
								pcdata " ";
								pcdata (Printer.Date.sprint "%-d %B %Y" date);
								pcdata "."
							];
							p [
								pcdata [%i18n S.schedule_message2]
							]
						]
						[pcdata [%i18n S.confirm ~capitalize:true]]
						[pcdata [%i18n S.cancel ~capitalize:true]] in
					if ok then
						let%lwt () = schedule_presentation (~%ayear, lw, ~%g, order = "first", ~%myid) in
						Os_lib.reload ()
					else
						Lwt.return_unit)
			)
		)
	] in
	match gnr with
	| None -> Moab_container.page (Some myid) [p [pcdata [%i18n S.no_group_number]]]
	| Some g -> let%lwt schedule_table = schedule_table (av_clicked g) myid ayear g weekday in
			Moab_container.page (Some myid) [
				div ~a:[a_class ["content-box"]] [
					h1 [pcdata [%i18n S.schedule_presentation]];
					p [pcdata [%i18n S.schedule_message]];
					schedule_table
				]
			]

let%shared view_schedule_handler myid (gnr) () =
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt sids = Moab_sessions.find_sessions (ayear, Seminar, Some gnr) in
	let%lwt weekday = Moab_sessions.get_session_weekday (List.hd sids) in
	let%lwt current_lw = Moab_terms.learning_week_of_date ayear (Date.today ()) >|= function None -> 0 | Some x -> x in
	let av_clicked = [%client (fun ev ->
		Js.Opt.case (ev##.target)
			(fun () -> ())
			(fun e -> Scanf.ksscanf (Js.to_string (e##.id))
				(fun _ _ -> ())
				"%d-%s"
				(fun lw order ->
					Lwt.async (fun () ->
						let ayear = ~%ayear in
						let gnr = ~%gnr in
						let%lwt x = get_random_unassigned_student (ayear, gnr, ~%current_lw) in
						match x with
						| None -> Lwt.return_unit
						| Some uid ->
							let%lwt () = schedule_presentation (ayear, lw, gnr, order = "first", uid) in
							(* let tr_o = Dom_html.getElementById_opt (Printf.sprintf "s%Ld" uid) in
							begin
								match tr_o with
								| None -> ()
								| Some tr -> let td::_ = Dom.list_of_nodeList tr##.childNodes in
									Dom.removeChild tr td
							end; *)
							Os_lib.reload ()
					)
				)
			)
		)
	] in
	let%lwt schedule_table = schedule_table av_clicked myid ayear gnr weekday in
	let%lwt student_trs = get_unassigned_students (ayear, gnr, current_lw) >>=
		Lwt_list.map_s (fun uid -> let%lwt u = Os_user_proxy.get_data uid in
			Lwt.return @@ tr ~a:[a_id (Printf.sprintf "s%Ld" uid)] [
				td [pcdata (Printf.sprintf "%s %s" u.fn u.ln)]
			]
		) in
	Moab_container.page (Some myid) [
		div ~a:[a_class ["content-box"]] [
			h1 [pcdata [%i18n S.schedule_for_group]; pcdata " "; pcdata (string_of_int gnr)];
			div ~a:[a_class ["flex-container"; "flex-row"]]
			[
				schedule_table;
				table ~a:[a_class ["unassigned-students-table"]] (tr [th [pcdata [%i18n S.unassigned_students]]]::student_trs)
			]
		]
	]

let%shared presentation_feedback_handler myid () () =
	Eliom_registration.Any.register ~service:presentation_feedback_action
		do_presentation_feedback;
	try%lwt
		let ayear = ~%(!Moab_config.current_academic_year) in
		let%lwt l = Moab_terms.learning_week_of_date ayear (Date.today ())	in
		let%lwt lw = match l with
			| None -> Lwt.fail_with [%i18n S.no_presentations_scheduled]
			| Some x -> Lwt.return x in
		let%lwt g = Moab_students.get_group_number (ayear, myid) in
		let%lwt gnr = match g with
			| None -> Lwt.fail_with [%i18n S.no_group_number]	
			| Some x -> Lwt.return x in
		let%lwt s = get_schedule (ayear, gnr) in
		let%lwt (p1, p2) = match List.assoc_opt (Int32.of_int lw) s with
		| None -> Lwt.fail_with [%i18n S.no_presentations_scheduled]
		| Some (None, None) -> Lwt.fail_with [%i18n S.no_presentations_scheduled]
		| Some (u1, u2) -> Lwt.return (u1, u2) in	
		let%lwt crits = get_criteria ayear in
		let pres_radio param uid fn ln =
			label [Form.radio ~name:param ~value:uid Form.int64; pcdata " "; pcdata fn; pcdata " "; pcdata ln] in
		let pres_select presenter_id = match p1, p2 with
			| None, None -> Lwt.fail_with [%i18n S.no_presentations_scheduled]
			| Some u1, None ->	
					let%lwt (fn, ln) = Moab_users.get_name u1 in
					Lwt.return [td [pres_radio presenter_id u1 fn ln]]
			| None, Some u2 ->	
					let%lwt (fn, ln) = Moab_users.get_name u2 in
					Lwt.return [td [pres_radio presenter_id u2 fn ln]]
			| Some u1, Some u2 ->
					let%lwt (fn1, ln1) = Moab_users.get_name u1 in
					let%lwt (fn2, ln2) = Moab_users.get_name u2 in
					Lwt.return [td [pres_radio presenter_id u1 fn1 ln1]; td [pres_radio presenter_id u2 fn2 ln2]] in
		let%lwt form = 
			Form.lwt_post_form ~service:presentation_feedback_action (fun (presenter_id, scores) ->
			let%lwt ps = pres_select presenter_id in
			Lwt.return [
				table [
					tr (ps)
				];
				table (scores.it (fun (crit_id) (id, text, descr) init ->
					tr [
						th [Form.input ~input_type:`Hidden ~name:crit_id ~value:id Form.int64; pcdata text]
					]::
					tr [
						th ~a:[a_class ["crit-description"]] [pcdata (match descr with None -> "" | Some x -> x)]
					]::
					init	
				) crits [])
			]) () in
		Moab_container.page (Some myid) [
			div ~a:[a_class ["content-box"]] [
				h1 [pcdata [%i18n S.presentation_feedback]];
				form
			]
		]
	with
	| Failure x -> Moab_container.page (Some myid) [p [pcdata x]]
	| e -> Lwt.fail e
