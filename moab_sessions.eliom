[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

(* Local services *)

let%server setup_sessions_action = Eliom_service.create_attached_post
	~fallback:Moab_services.setup_sessions_service
	~post_params:(any)
	()

let%client setup_sessions_action = 
	~%setup_sessions_action

(* Database access *)

let%server get_sessions ayear =
	Moab_session_db.get_sessions ayear

let%client get_sessions =
	~%(Eliom_client.server_function [%derive.json : string]
		(Os_session.connected_wrapper get_sessions))

(* Handlers *)

let%server do_setup_sessions () params =
	List.iter (fun (n, v) ->
		Ocsigen_messages.console (fun () -> Printf.sprintf "%s = %s" n v)
	) params;
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared real_setup_sessions_handler myid () () =
	let%lwt ayears = Moab_terms.get_academic_years () in
	let%lwt sessions = match ayears with
	| [] -> Lwt.return []
	| h::_ -> let%lwt l = get_sessions h in
			let%lwt t = Moab_terms.get_term_ids h in
			Lwt.return @@ List.mapi (fun i (tid, sid, stype, w, st, et, r) -> (i, tid, sid, stype, w, st, et, r)) l in
	let (session_l, session_h) = Eliom_shared.ReactiveData.RList.create sessions in
	let ayear_change_function t =
		ignore [%client ((Lwt.async @@ fun () ->
			let sel = Eliom_content.Html.To_dom.of_element ~%t in
			Lwt_js_events.changes sel @@ fun _ _ ->
			Js.Opt.case (Dom_html.CoerceTo.select sel)
				(fun () -> Lwt.return_unit)
			  (fun s ->
					let ayear = Js.to_string s##.value in
					let%lwt sessions = get_sessions ayear in
					Eliom_shared.ReactiveData.RList.set ~%session_h
						(List.mapi (fun i (tid, sid, stype, w, st, et, r) -> (i, tid, sid, stype, w, st, et, r)) sessions);
					Lwt.return_unit
				)
			) : unit)
		] in
	let display_session_rows (l, h) =
		Eliom_shared.ReactiveData.RList.map 
			[%shared ((fun (nr, term_id, session_id, session_type, weekday, start_time, end_time, room) -> 
				D.tr [
					D.td [Raw.input ~a:[a_name (Printf.sprintf "term_id[%d]" nr); a_input_type `Text; a_value (Int64.to_string term_id)] ()];
					D.td [pcdata session_type];
					D.td [pcdata (Printer.name_of_day (Date.day_of_int weekday))];
					D.td [pcdata (Printer.Time.sprint "%H:%M" start_time)];
					D.td [pcdata (Printer.Time.sprint "%H:%M" end_time)];
					D.td [pcdata (match room with None -> [%i18n S.tbd] | Some x -> x)]
				]
			): _ -> _)] l in
	let%lwt session_form = Form.lwt_post_form ~service:setup_sessions_action (fun params ->
		let%lwt ayear_widget = Moab_terms.academic_year_select_widget (`String "academic_year") in
		let session_rows = display_session_rows (session_l, session_h) in
		ayear_change_function ayear_widget;
		Lwt.return [
			Eliom_content.Html.R.table ~thead:(Eliom_shared.React.S.const (thead [
				tr [
					th [pcdata [%i18n S.academic_year]];
					td [ayear_widget]
				];
				tr [
					th [pcdata [%i18n S.term]];
					th [pcdata [%i18n S.session_type]];
					th [pcdata [%i18n S.weekday]];
					th [pcdata [%i18n S.start_time]];
					th [pcdata [%i18n S.end_time]];
					th [pcdata [%i18n S.room]];
				]
			])) ~tfoot:(Eliom_shared.React.S.const (tfoot [
				tr [
					td ~a:[a_colspan 6] [Raw.input ~a:[a_class ["button"]; a_input_type `Submit; a_value [%i18n S.save]] ()]
				]
			]))
			 session_rows
		]) () in
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]] [
			h1 [pcdata [%i18n S.setup_sessions ~capitalize:true]];
			session_form
		]
	]

let%server setup_sessions_handler myid () () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:setup_sessions_action do_setup_sessions;
	real_setup_sessions_handler myid () ()

let%client setup_sessions_handler =
	real_setup_sessions_handler
