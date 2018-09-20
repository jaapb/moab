[%%shared
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

(* Local services *)

let%server add_students_action =
	Eliom_service.create_attached_post
		~fallback:Moab_services.add_students_service
		~post_params:(string "academic_year" ** string "group" ** file "csv")
		()

let%client add_students_action = 
	~%add_students_action

let%server add_students_action2 =
	Eliom_service.create_attached_post
		~fallback:Moab_services.add_students_service
		~post_params:(string "academic_year" ** list "changes" (bool "do" ** string "action" ** opt (int64 "uid") ** string "first_name" ** string "last_name" ** string "mdx_id" ** string "email"))
		()

let%client add_students_action2 = 
	~%add_students_action2

(* Database access *)

let%server get_group_numbers ayear =
	Moab_student_db.get_group_numbers ayear

let%client get_group_numbers =
	~%(Eliom_client.server_function [%derive.json : string]
			(Os_session.connected_wrapper get_group_numbers))

let%server find_student mdx_id =
	Moab_student_db.find_student mdx_id

let%client find_student =
	~%(Eliom_client.server_function [%derive.json : string]
			(Os_session.connected_wrapper find_student))

let%server find_student_opt mdx_id =
	try%lwt
		let%lwt uid = Moab_student_db.find_student mdx_id in
		Lwt.return_some uid
	with
	|  Not_found -> Lwt.return_none

let%client find_student_opt =
	~%(Eliom_client.server_function [%derive.json : string]
			(Os_session.connected_wrapper find_student_opt))

let%server get_students ayear =
	Moab_student_db.get_students ayear

let%client get_students =
	~%(Eliom_client.server_function [%derive.json : string]
			(Os_session.connected_wrapper get_students))

(* Database access *)

let%server set_student_info (uid, ayear, mdx_id, joined_week) =
	Moab_student_db.set_student_info uid ayear mdx_id joined_week

let%client set_student_info =
	~%(Eliom_client.server_function [%derive.json : int64 * string * string * int]
		(Os_session.connected_wrapper set_student_info))

let%server get_group_number (ayear, uid) =
	Moab_student_db.get_group_number ayear uid

let%client get_group_number =
	~%(Eliom_client.server_function [%derive.json : string * int64]
		(Os_session.connected_wrapper get_group_number))

let%server set_group_number (ayear, uid, gnr) =
	Moab_student_db.set_group_number ayear uid gnr

let%client set_group_number =
	~%(Eliom_client.server_function [%derive.json : string * int64 * int option]
		(Os_session.connected_wrapper set_group_number))

(* Handlers *)

let%server do_add_students2 myid () (ayear, changes_list) =
	Ocsigen_messages.console (fun () -> Printf.sprintf "[do_add_students2] ay: %s cl: %d" ayear (List.length changes_list));
	let%lwt () = Lwt_list.iter_s (fun (do_b, (act, (uid, (fn, (ln, (mdx_id, email)))))) ->
		if do_b then
		begin
			try%lwt
				Scanf.sscanf act "group_%s" (fun g ->
					match uid with
					| None -> Lwt.fail (Invalid_argument "group_<nr> action, but no uid")
					| Some u -> let gp = if g = "none" then None else Some (int_of_string g) in
							set_group_number (ayear, u, gp));
				Scanf.sscanf act "join_%d" (fun w ->
					let%lwt uid = Moab_users.add_user (Student, fn, ln, email, Some mdx_id) in
					let%lwt () = set_student_info (uid, ayear, mdx_id, w) in
					Lwt.return_unit)
			with
			| Scanf.Scan_failure _ -> Lwt.return_unit
		end
		else
			Lwt.return_unit
	) changes_list	in
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)
			
let%server do_add_students myid () (ayear_v, (group, csv)) =
	Ocsigen_messages.console (fun () -> Printf.sprintf "[do_add_students] ay: %s g: %s csv: %s" ayear_v group csv.Ocsigen_extensions.tmp_filename);
	let%lwt lwo = Moab_terms.learning_week_of_date ayear_v (Date.today ()) in
	let lw = match lwo with
		| None -> 1
		| Some x -> x in
	let%lwt f = Lwt_io.open_file ~mode:Lwt_io.Input csv.Ocsigen_extensions.tmp_filename in
	let%lwt c = Csv_lwt.of_channel f in
	let%lwt act_list = Csv_lwt.fold_left (fun acc l ->
		let _::name::mdx_id::_::_::_::_::_::_::_::mail::[] = l in
		Scanf.sscanf name "%s@, %s" (fun ln fn ->
			let name = Printf.sprintf "%s %s" fn ln in
			Scanf.sscanf mail "mailto:%s" (fun e ->
				try%lwt
					let%lwt uid = Moab_users.find_user e in
					let%lwt st_group = get_group_number (ayear_v, uid) in
					Ocsigen_messages.console (fun () -> Printf.sprintf "> [%s] sg: %s" mdx_id (match st_group with None -> "<none>" | Some x -> string_of_int x));
					if group <> "" then
						match st_group with
						| Some sg when group <> string_of_int sg ->
							Lwt.return @@ (`To_group (int_of_string group), Some uid, fn, ln, mdx_id, e)::acc
						| None -> Lwt.return @@ (`To_group (int_of_string group), Some uid,  fn, ln, mdx_id, e)::acc
						| _ -> Lwt.return @@ acc
					else	
						match st_group with
						| Some _ -> Lwt.return @@ (`No_group, Some uid, fn, ln, mdx_id, e)::acc
						| _ -> Lwt.return @@ acc
				with
				| Not_found -> Lwt.return @@ (`New lw, None, fn, ln, mdx_id, e)::acc
			)
		)
	) [] c in
	let%lwt () = Csv_lwt.close_in c in
	Moab_container.page (Some myid) 
	[
		div ~a:[a_class ["content-box"]]
		[
			h1 [pcdata [%i18n S.changes_to_be_made]];
			Form.post_form ~service:add_students_action2 (fun (ayear, changes_list) ->
				[Form.input ~input_type:`Hidden ~name:ayear ~value:ayear_v Form.string;
				table (
					tr [th []; th [pcdata [%i18n S.action]]; th [pcdata [%i18n S.name]]; th [pcdata [%i18n S.student_id]]; th [pcdata [%i18n S.email_address]]]::
					changes_list.it (fun (do_b, (act, (uid, (fn, (ln, (mdx_id, email)))))) (act_v, uid_ov, fn_v, ln_v, mdx_id_v, email_v) init ->
						tr [
							td [Form.bool_checkbox_one ~checked:true ~name:do_b ()];
							td (match act_v with
							| `New x -> [
									Form.input ~input_type:`Hidden ~name:act ~value:(Printf.sprintf "join_%d" x) Form.string;
									pcdata [%i18n S.joining_week]; pcdata " "; pcdata (string_of_int x)
								]
							| `No_group -> (
									pcdata [%i18n S.no_group]::
									Form.input ~input_type:`Hidden ~name:act ~value:"group_none" Form.string::
									(match uid_ov with
									| None -> []
									| Some u -> [Form.input ~input_type:`Hidden ~name:uid ~value:u Form.int64])
								)
							| `To_group x -> (
									pcdata [%i18n S.to_group]::pcdata " "::pcdata (string_of_int x)::
									Form.input ~input_type:`Hidden ~name:act ~value:(Printf.sprintf "group_%d" x) Form.string::
									(match uid_ov with
									| None -> []
									| Some u -> [Form.input ~input_type:`Hidden ~name:uid ~value:u Form.int64])
								)
							);
							td [pcdata fn_v; pcdata " "; pcdata ln_v;
								Form.input ~input_type:`Hidden ~name:fn ~value:fn_v Form.string;
								Form.input ~input_type:`Hidden ~name:ln ~value:ln_v Form.string	
							];
							td [pcdata mdx_id_v;
								Form.input ~input_type:`Hidden ~name:mdx_id ~value:mdx_id_v Form.string
							];
							td [pcdata email_v;
								Form.input ~input_type:`Hidden ~name:email ~value:email_v Form.string
							]
						]::
						init	
					) act_list
					[
						tr [td ~a:[a_colspan 2] [Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:[%i18n S.save] Form.string]]
					]
				)]
			) ()
		]
	]

let%shared real_add_students_handler myid () () =
	let%lwt student_form = Form.lwt_post_form ~service:add_students_action (fun (ayear, (group, csv)) ->
		let%lwt ayear_widget = Moab_terms.academic_year_select_widget (`Param ayear) in
		Lwt.return [
			table
			[
				tr
				[
					th [pcdata [%i18n S.academic_year]];
					td [ayear_widget]
				];
				tr
				[
					th [pcdata [%i18n S.group_number]];
					td [Form.input ~name:group ~input_type:`Text Form.string]
				];
				tr 
				[
					th [pcdata [%i18n S.csv_file_from_misis]];
					td [Form.file_input ~name:csv ()]
				];
				tr
				[ 
					td ~a:[a_colspan 2] [Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:[%i18n S.process] Form.string]
				]
			]
		]) () in
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]]
		[
			h1 [pcdata [%i18n S.add_students ~capitalize:true]];
			student_form
		]
	]

let%server add_students_handler myid () () =
	Ocsigen_messages.console (fun () -> "[add_students]");
	Moab_base.App.register ~scope:Eliom_common.default_session_scope
		~service:add_students_action (Moab_page.connected_page do_add_students);
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:add_students_action2 (Os_session.connected_fun do_add_students2);
	real_add_students_handler myid () ()

let%client add_students_handler =
	real_add_students_handler
