[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

(* Local services *)

let%server generate_attendance_report_action = Eliom_service.create_attached_post
	~fallback:Moab_services.generate_attendance_report_service
	~post_params:(int "start_week" ** int "end_week")
	()

let%client generate_attendance_report_action = 
	~%generate_attendance_report_action

(* Database access *)

let%server get_session_attendance (sid, lw) =
	Moab_attendance_db.get_session_attendance sid lw

let%client get_session_attendance =
	~%(Eliom_client.server_function [%derive.json : int64 * int]
		(Os_session.connected_wrapper get_session_attendance))

let%server add_session_attendance (sid, uid, lw) =
	Moab_attendance_db.add_session_attendance sid uid lw

let%client add_session_attendance =
	~%(Eliom_client.server_function [%derive.json : int64 * int64 * int]
		(Os_session.connected_wrapper add_session_attendance))

let%server get_week_attendance (uid, ayear, term, lw) =
	Moab_attendance_db.get_week_attendance uid ayear term lw

let%client get_week_attendance =
	~%(Eliom_client.server_function [%derive.json : int64 * string * int64 * int]
		(Os_session.connected_wrapper get_week_attendance))

let%server get_attendance_list (ayear, lw) =
	Moab_attendance_db.get_attendance_list ayear lw

let%client get_attendance_list =
	~%(Eliom_client.server_function [%derive.json : string * int]
		(Os_session.connected_wrapper get_attendance_list))

(* Utility functions *)

let%shared attendance_tr uid =
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt weeks = Moab_terms.get_learning_weeks ayear in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in 
	let%lwt (jw, lfw0) = Moab_students.get_active_period (ayear, uid) in
	let lfw = match lfw0 with None -> (List.length weeks) + 1 | Some x -> x in
	let%lwt week_list = Lwt_list.mapi_s (fun i (t, w, y) ->
		let week_nr = i + 1 in
		let%lwt (a, s) = get_week_attendance (uid, ayear, t, week_nr) in
		let att_class = 
			match lw with
			| None -> []
			| Some learning_week ->
				if week_nr > learning_week then []
				else if week_nr < jw then []
				else if week_nr > lfw then []
				else if a = s then ["dt-good"]
				else if a = 0 then ["dt-bad"]
				else ["dt-warning"] in
		Lwt.return @@	td ~a:[a_class att_class] [pcdata (string_of_int week_nr)]
	) weeks in
	Lwt.return @@ tr (
		td [b [pcdata [%i18n S.your_attendance]]; pcdata " "]::
		week_list
	)

let%shared attendance_report () =
	let fits lw jw lfw =
		match lfw with
		| None -> jw <= lw
		| Some x -> jw <= lw && lw <= x in
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt x = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let lw = match x with
	| None -> 0 
	| Some l -> l in
	let%lwt l = get_attendance_list (ayear, lw) in
	let%lwt att_list = Lwt_list.map_s (fun (uid, pos, att) ->
		let%lwt (fn, ln) = Moab_users.get_name uid in
		let%lwt sid = Moab_students.get_student_id uid in
		let%lwt (jw, lw) = Moab_students.get_active_period (ayear, uid) in
		Lwt.return (Printf.sprintf "%s %s" fn ln, sid, (if pos = 0L then 0 else Int64.to_int (Int64.div (Int64.mul att 100L) pos)), jw, lw)
	) l in
	Lwt.return @@ table (List.map (fun (name, student_id, perc, _, _) ->
		tr [
			td [pcdata name];
			td [pcdata student_id];
			td [pcdata (string_of_int perc)]
		]
	) (List.sort (fun (_, _, x, _, _) (_, _, y, _, _) -> compare x y)
			(List.filter (fun (_, _, p, j, l) -> p < 25 && (fits lw j l)) att_list)))

(* Handlers *)

(* we have to do this this way because Eliom_registration.File is not available on the
 * client *)
let%server do_generate_attendance_report myid () (start_week, end_week) =
	let ayear = !Moab_config.current_academic_year in
	let%lwt x = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let lw = match x with
	| None -> 0
	| Some l -> l in
	let%lwt lws = Moab_terms.get_learning_weeks ayear in
	let%lwt students = Moab_students.get_active_students (ayear, lw, None) in
	let%lwt csv = Lwt_list.mapi_s (fun n (_, w, y) ->
		let i = n + 1 in
		Lwt_list.map_s (fun uid ->
			let%lwt u = Os_user_proxy.get_data uid in
			Lwt.return [string_of_int i; ""; ""; ""; ""; ""; ""; u.fn; u.ln; ""; ""; ""]	
		) students
	) lws in
	let tmpnam = Filename.temp_file "moab_report" ".csv" in
	let%lwt out_ch = Lwt_io.open_file ~flags:[O_WRONLY; O_CREAT; O_TRUNC] ~mode:Output tmpnam in
	let csv_ch = Csv_lwt.to_channel out_ch in
	let csv_header = ["Week number"; "Scheduled sessions"; "Student number"; "Sessions attended"; "Week starting"; "Tutor";
		"Network name"; "First Name"; "Last Name"; "Email"; "Visa?"; "Foundation?"] in
	let%lwt () = Csv_lwt.output_all csv_ch (csv_header::List.flatten csv) in
	let%lwt () = Csv_lwt.close_out csv_ch in
	Eliom_registration.File.send ~content_type:"text/csv" tmpnam

let%shared real_generate_report_handler myid () () =
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]] [
			h1 [pcdata [%i18n S.generate_attendance_report]];
			Form.post_form ~service:generate_attendance_report_action
			(fun (start_week, end_week) -> [
				label [
					pcdata "Start week";
					Form.input ~name:start_week ~input_type:`Text Form.int
				];
				label [
					pcdata "End week";
					Form.input ~name:end_week ~input_type:`Text Form.int
				];
		 		Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:"Generate" Form.string
			]) ()
		]
	]

let%server generate_report_handler myid () () =
	Eliom_registration.Any.register ~service:generate_attendance_report_action
		(Os_session.connected_fun do_generate_attendance_report);
	real_generate_report_handler myid () ()

let%client generate_report_handler =
	real_generate_report_handler
	
let%shared register_attendance_handler myid () () =
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt csids = Moab_sessions.get_current_sessions ayear in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let learning_week = match lw with
		| None -> 0
		| Some x -> x in
	let%lwt sid = match csids with
		| [] ->
			let%lwt ct = Moab_terms.get_current_term ayear in
			let%lwt tsids = Moab_sessions.get_sessions (ayear, Some ct) in
			(match tsids with
			| [] -> Lwt.return_none
			| (_,h,_,_,_,_,_,_)::_ -> Lwt.return_some h)
		| x::_ -> Lwt.return_some x in
	let%lwt attendance = match sid with
		| None -> Lwt.return []
		| Some x -> get_session_attendance (x, learning_week) in
	let (sid_s, sid_f) = Eliom_shared.React.S.create sid in
	let (attendance_l, attendance_h) = Eliom_shared.ReactiveData.RList.create attendance in
	let attendance_rows l = Eliom_shared.ReactiveData.RList.map 
		[%shared ((fun (uid, mdx_id, fn, ln) ->
			tr [
				td [Moab_icons.D.trash ()];
				td [pcdata mdx_id];
				td [pcdata fn; pcdata " "; pcdata ln]
			]
		): _ -> _)] l in
	let%lwt session_id_select = Moab_sessions.session_select_widget ?current_sid:sid ayear in
	let student_id_input = D.Raw.input () in
	let fplayer = Eliom_content.Html.D.(audio 
		~srcs:[source ~a:[a_src (D.make_uri ~service:(Eliom_service.static_dir ()) ["failure.wav"])] ()]
		[pcdata "alt"]) in
	let splayer = Eliom_content.Html.D.(audio 
		~srcs:[source ~a:[a_src (D.make_uri ~service:(Eliom_service.static_dir ()) ["success.wav"])] ()]
		[pcdata "alt"]) in
	ignore [%client ((Lwt.async @@ fun () ->
		let student_id_regexp = Re.Str.regexp "M[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]" in
		let splayer = Eliom_content.Html.To_dom.of_audio ~%splayer in
		let fplayer = Eliom_content.Html.To_dom.of_audio ~%fplayer in
		let inp = Eliom_content.Html.To_dom.of_input ~%student_id_input in
		Lwt_js_events.changes inp @@ fun _ _ ->
			let student_id = String.uppercase_ascii (Js.to_string inp##.value) in
			let%lwt () =
			if Re.Str.string_match student_id_regexp student_id 0 = true
			then let%lwt x = Moab_students.find_student_opt student_id in
			begin
				match x with
				| None ->
						(fplayer##play;
						Os_msg.msg ~level:`Err [%i18n S.unknown_student];
						Lwt.return_unit)
				|	Some uid ->
					begin
						match Eliom_shared.React.S.value ~%sid_s with
						| None -> Lwt.return_unit
						| Some x ->
							let%lwt (fn, ln) = Moab_users.get_name uid in
							let%lwt () = add_session_attendance (x, uid, ~%learning_week) in
							Eliom_shared.ReactiveData.RList.snoc (uid, student_id, fn, ln) ~%attendance_h;
							splayer##play;
							Lwt.return_unit
					end
			end
			else
			begin
				Os_msg.msg ~level:`Err [%i18n S.invalid_student_id];
				Lwt.return_unit
			end in
			inp##.value := Js.string "";
			inp##focus;
			Lwt.return_unit
	): unit)];
	ignore [%client ((Lwt.async @@ fun () ->
		let inp = Eliom_content.Html.To_dom.of_input ~%student_id_input in
		let sel = Eliom_content.Html.To_dom.of_select ~%session_id_select in
		Lwt_js_events.changes sel @@ fun _ _ ->
			let new_sid = Int64.of_string (Js.to_string sel##.value) in
			~%sid_f (Some new_sid);
			let%lwt new_att = get_session_attendance (new_sid, ~%learning_week) in
			Eliom_shared.ReactiveData.RList.set ~%attendance_h new_att;
			inp##focus;
			Lwt.return_unit
	): unit)];
	let focus_f = [%client (fun _ -> 
		let inp = Eliom_content.Html.To_dom.of_input ~%student_id_input in
		inp##focus
	)] in
	Moab_container.page ~a:[a_onload focus_f] (Some myid)
	[
		splayer;
		fplayer;
		div ~a:[a_class ["content-box"]] [
			h1 [pcdata [%i18n S.register_attendance]];
			table [
				tr [
					td [session_id_select]
				];
				tr [
					td [b [pcdata [%i18n S.student_id]]; pcdata " "; student_id_input]
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
