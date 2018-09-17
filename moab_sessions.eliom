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

let%server get_fresh_session_id () =
	Moab_session_db.get_fresh_session_id ()

let%client get_fresh_session_id =
	~%(Eliom_client.server_function [%derive.json : unit]
		(Os_session.connected_wrapper get_fresh_session_id))

let%server add_or_update_session (session_id, ayear, term_id, session_type, weekday, start_time, end_time, room, group_number) =
	Moab_session_db.add_session session_id ayear term_id session_type weekday start_time end_time room group_number

let%client add_or_update_session =
	~%(Eliom_client.server_function [%derive.json : int64 option * string * int64 * string * int * string * string * string option *
			int option]
		(Os_session.connected_wrapper add_or_update_session))

let%server get_current_sessions ayear =
	Moab_session_db.get_current_sessions ayear

let%client get_current_sessions =
	~%(Eliom_client.server_function [%derive.json : string]
		(Os_session.connected_wrapper get_current_sessions))

(* Handlers *)

let%server do_setup_sessions () params =
	Ocsigen_messages.console (fun () -> "[do_setup_sessions]");
	let ayear = List.assoc "academic_year" params in
	let h = Hashtbl.create (List.length params) in
	let sid_list = ref [] in
	List.iter (fun (n, v) ->
		Scanf.ksscanf n
			(fun c _ -> ())
			"%s@[%Ld]" (fun tp sid -> 
				if tp = "term_id" then (sid_list := sid::!sid_list; Hashtbl.add h (sid, `Term_id) v)
				else if tp = "session_type" then Hashtbl.add h (sid, `Session_type) v
				else if tp = "weekday" then Hashtbl.add h (sid, `Weekday) v
				else if tp = "start_time" then Hashtbl.add h (sid, `Start_time) v
				else if tp = "end_time" then Hashtbl.add h (sid, `End_time) v
				else if tp = "room" then Hashtbl.add h (sid, `Room) v
				else if tp = "group_number" then Hashtbl.add h (sid, `Group_number) v
				else ()
			)
	) params;
	let%lwt () = Lwt_list.iter_s (fun sid ->
		let room = Hashtbl.find h (sid, `Room) in
		let gnr = Hashtbl.find h (sid, `Group_number) in
		add_or_update_session (Some sid, ayear, Int64.of_string (Hashtbl.find h (sid, `Term_id)),
			Hashtbl.find h (sid, `Session_type), int_of_string (Hashtbl.find h (sid, `Weekday)),
			Hashtbl.find h (sid, `Start_time), Hashtbl.find h (sid, `End_time),
			(if room = "" then None else Some room), if gnr = "" then None else Some (int_of_string gnr))
	) !sid_list in
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared term_selector nr v terms =
	R.select ~a:[a_name (Printf.sprintf "term_id[%Ld]" nr)]
		(Eliom_shared.ReactiveData.RList.map 
			[%shared ((fun x -> if x = ~%v
			then option ~a:[a_selected ()] (pcdata (Int64.to_string x))
			else option (pcdata (Int64.to_string x))): _ -> _)] terms)

let%shared session_selector nr v =
	Raw.select ~a:[a_name (Printf.sprintf "session_type[%Ld]" nr)] 
		(List.map (fun x -> if x = v
			then option ~a:[a_selected ()] (pcdata x)
			else option (pcdata x)) ["L"; "S"])

let%shared weekday_selector nr v =
	Raw.select ~a:[a_name (Printf.sprintf "weekday[%Ld]" nr)]
		(List.map (fun x -> if x = v
			then option ~a:[a_selected (); a_value (string_of_int x)] (pcdata (Printer.name_of_day (Date.day_of_int x)))
			else option ~a:[a_value (string_of_int x)] (pcdata (Printer.name_of_day (Date.day_of_int x)))) [1; 2; 3; 4; 5; 6; 7])

let%shared group_number_selector nr v groups =
	R.select ~a:[a_name (Printf.sprintf "group_number[%Ld]" nr)]
		(Eliom_shared.ReactiveData.RList.map
			[%shared ((fun x -> if x = ~%v
			then option ~a:[a_selected ()] (pcdata x)
			else option (pcdata x)): _ -> _)] groups)
 
let%shared sessions_aux terms groups (tid, sid, t, wd, st, et, rm, gn) =
 (term_selector sid tid terms, sid, t, wd, Some st, Some et, rm,
	group_number_selector sid (match gn with None -> "" | Some x -> string_of_int x) groups)

let%shared time_or_empty = function
| None -> ""
| Some t -> Printer.Time.sprint "%H:%M" t
	
let%shared real_setup_sessions_handler myid () () =
	let%lwt ayears = Moab_terms.get_academic_years () in
	let%lwt terms = match ayears with
	| [] -> Lwt.return []
	| h::_ -> Moab_terms.get_term_ids h in
	let (term_l, term_h) = Eliom_shared.ReactiveData.RList.create terms in
	let%lwt groups = match ayears with
	| [] -> Lwt.return [""]
	| h::_ -> let%lwt gn = Moab_students.get_group_numbers h in
			Lwt.return @@ ""::(List.map string_of_int gn) in
	let (group_l, group_h) = Eliom_shared.ReactiveData.RList.create groups in
	let%lwt sessions = match ayears with
	| [] -> Lwt.return []
	| h::_ -> let%lwt l = get_sessions h in
		Lwt.return @@ List.map (sessions_aux term_l group_l) l in
	let (session_l, session_h) = Eliom_shared.ReactiveData.RList.create sessions in
	let display_session_rows l =
		Eliom_shared.ReactiveData.RList.map
			[%shared ((fun (ts, session_id, session_type, weekday, start_time, end_time, room, gns) -> 
				D.tr [
					D.td [];
					D.td [ts];
					D.td [session_selector session_id session_type];
					D.td [weekday_selector session_id weekday];
					D.td [Raw.input ~a:[a_name (Printf.sprintf "start_time[%Ld]" session_id); a_input_type `Time; a_value (time_or_empty start_time)] ()];
					D.td [Raw.input ~a:[a_name (Printf.sprintf "end_time[%Ld]" session_id); a_input_type `Time; a_value (time_or_empty end_time)] ()];
					D.td [Raw.input ~a:[a_name (Printf.sprintf "room[%Ld]" session_id); a_input_type `Text; a_value (match room with None -> "" | Some x -> x)] ()];
					D.td [gns]
				]
			): _ -> _)] l in
	let%lwt session_form = Form.lwt_post_form ~service:setup_sessions_action (fun params ->
		let%lwt ayear_widget = Moab_terms.academic_year_select_widget (`String "academic_year") in
		ignore [%client ((Lwt.async @@ fun () ->
			let sel = Eliom_content.Html.To_dom.of_element ~%ayear_widget in
			Lwt_js_events.changes sel @@ fun _ _ ->
			Js.Opt.case (Dom_html.CoerceTo.select sel)
				(fun () -> Lwt.return_unit)
			  (fun s ->
					let ayear = Js.to_string s##.value in
					let%lwt sessions = get_sessions ayear in
					let%lwt terms = Moab_terms.get_term_ids ayear in
					let%lwt gn = Moab_students.get_group_numbers ayear in
					let groups = ""::(List.map string_of_int gn) in
					Eliom_shared.ReactiveData.RList.set ~%term_h terms;
					Eliom_shared.ReactiveData.RList.set ~%group_h groups;
					Eliom_shared.ReactiveData.RList.set ~%session_h (List.map (sessions_aux ~%term_l ~%group_l) sessions);
					Lwt.return_unit
				)
			) : unit)
		];
		let add_button = Moab_icons.D.add () in
		ignore [%client ((Lwt.async @@ fun () ->
			let btn = Eliom_content.Html.To_dom.of_element ~%add_button in
			Lwt_js_events.clicks btn @@ fun _ _ ->
			let%lwt sid = get_fresh_session_id () in
			Eliom_shared.ReactiveData.RList.snoc (term_selector sid 1L ~%term_l, sid, "L", 1, None, None, None,
			group_number_selector sid "" ~%group_l) ~%session_h;
			Lwt.return_unit
			) : unit)
		];
		let session_rows = display_session_rows session_l in
		Lwt.return [
			Eliom_content.Html.R.table ~thead:(Eliom_shared.React.S.const (thead [
				tr [
					th ~a:[a_colspan 2] [pcdata [%i18n S.academic_year]];
					td ~a:[a_colspan 5] [ayear_widget]
				];
				tr [
					th [];	
					th [pcdata [%i18n S.term]];
					th [pcdata [%i18n S.session_type]];
					th [pcdata [%i18n S.weekday]];
					th [pcdata [%i18n S.start_time]];
					th [pcdata [%i18n S.end_time]];
					th [pcdata [%i18n S.room]];
				]
			])) ~tfoot:(Eliom_shared.React.S.const (tfoot [
				tr [
					td [add_button];
					td ~a:[a_colspan 6] [];
				];
				tr [
					td ~a:[a_colspan 7] [Raw.input ~a:[a_class ["button"]; a_input_type `Submit; a_value [%i18n S.save]] ()]
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
