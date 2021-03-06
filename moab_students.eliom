[%%shared
	open Eliom_parameter
	open CalendarLib
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Lwt
]

(* Local types *)

[%%shared
	type to_do_type = Deactivate of int | New of int | To_group of int
	[@@deriving json]
]

(* Local services *)

let%server add_students_action =
	Eliom_service.create_attached_post
		~name:"add_students_action"
		~fallback:Moab_services.add_students_service
		~post_params:(string "academic_year" ** string "group" ** file "csv")
		()

let%client add_students_action = 
	~%add_students_action

let%server add_students_action2 =
	Eliom_service.create_attached_post
		~name:"add_students_action2"
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

let%server get_students (ayear, gnr, lw) =
	Moab_student_db.get_students ayear gnr lw

let%client get_students =
	~%(Eliom_client.server_function [%derive.json : string * int option * int option]
			(Os_session.connected_wrapper get_students))

let%server get_student_id uid =
	Moab_student_db.get_student_id uid

let%client get_student_id =
	~%(Eliom_client.server_function [%derive.json: int64]
			(Os_session.connected_wrapper get_student_id))

let%server get_active_students (ayear, week, gnr) =
	Moab_student_db.get_active_students ayear week gnr

let%client get_active_students =
	~%(Eliom_client.server_function [%derive.json : string * int * int option]
			(Os_session.connected_wrapper get_active_students))

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

let%server deactivate_student (ayear, uid, week) =
	Moab_student_db.deactivate_student ayear uid week

let%client deactivate_student =
	~%(Eliom_client.server_function [%derive.json : string * int64 * int]
		(Os_session.connected_wrapper deactivate_student))

let%server get_active_period (ayear, uid) =
	Moab_student_db.get_active_period ayear uid

let%client get_active_period =
	~%(Eliom_client.server_function [%derive.json : string * int64]
		(Os_session.connected_wrapper get_active_period))

(* Widgets *)
let%shared student_select_widget param =
	let student_opt (id, fn, ln) =
		D.Form.Option ([], id, Some (txt (Printf.sprintf "%s %s" fn ln)), false) in
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt current_lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let%lwt students = get_students (ayear, None, current_lw) in
	let%lwt sns = Lwt_list.map_s (fun uid ->
		let%lwt (fn, ln) = Moab_users.get_name uid in
		Lwt.return (uid, fn, ln)
	) students in
	match sns with
	| [] -> Lwt.return (D.Raw.select [])
	| h::t -> begin
		match param with
		| `Param p -> Lwt.return @@ D.Form.select ~name:p Form.int64 (student_opt h) (List.map student_opt t)
		| `String s -> Lwt.return @@ D.Raw.select ~a:[a_name s] (List.map (fun (id, fn, ln) -> option ~a:[a_value (Int64.to_string id)] (txt (Printf.sprintf "%s %s" fn ln))) (h::t))
	end

(* Handlers *)

let%shared do_add_students2 myid () (ayear, changes_list) =
	let%lwt () = Lwt_list.iter_s (fun (do_b, (act, (uid, (fn, (ln, (mdx_id, email)))))) ->
		if do_b then
		begin
			try%lwt
				Scanf.sscanf act "group_%s" (fun g ->
					match uid with
					| None -> Lwt.fail (Invalid_argument "group_<nr> action, but no uid")
					| Some u -> set_group_number (ayear, u, Some (int_of_string g)));
			with
			| Scanf.Scan_failure _ ->
			begin
				try%lwt
					Scanf.sscanf act "join_%d" (fun w ->
						let%lwt uid = Moab_users.add_user (Student, fn, ln, email, Some mdx_id) in
						let%lwt () = set_student_info (uid, ayear, mdx_id, w) in
						Lwt.return_unit)
				with
				| Scanf.Scan_failure _ -> 
				begin
					try%lwt
						Scanf.sscanf act "deactivate_%d" (fun w ->
							match uid with
							| None -> Lwt.fail (Invalid_argument "deactivate_<nr> action, but no uid")
							| Some u -> deactivate_student (ayear, u, w)
						)
					with
					| Scanf.Scan_failure _ -> Lwt.return_unit
				end
			end
		end
		else
			Lwt.return_unit
	) changes_list	in
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)
			
let%server read_students_csv_file (fn, ayear_v, group, lw) =
	let%lwt f = Lwt_io.open_file ~mode:Lwt_io.Input fn in
	let%lwt c = Csv_lwt.of_channel f in
	let%lwt stud_list = get_students (ayear_v, None, lw) in
	let rem_hashtbl = Hashtbl.create (List.length stud_list) in
		List.iter (fun uid ->
			Hashtbl.add rem_hashtbl uid ()
		) stud_list;
	let%lwt act_list0 = Csv_lwt.fold_left (fun acc l ->
		let _::name::mdx_id::_::_::_::_::_::_::_::mail::[] = l in
		Scanf.sscanf name "%s@, %s" (fun ln fn ->
			let name = Printf.sprintf "%s %s" fn ln in
			Scanf.sscanf mail "mailto:%s" (fun e ->
				try%lwt
					let%lwt uid = Moab_users.find_user e in
					let%lwt st_group = get_group_number (ayear_v, uid) in
					Hashtbl.remove rem_hashtbl uid;
					if group <> "" then
						match st_group with
						| Some sg when group <> string_of_int sg ->
							Lwt.return @@ (To_group (int_of_string group), Some uid, fn, ln, mdx_id, e)::acc
						| None -> Lwt.return @@ (To_group (int_of_string group), Some uid, fn, ln, mdx_id, e)::acc
						| _ -> Lwt.return @@ acc
					else	
						match st_group with
						| Some _ -> Lwt.return @@ acc 
						| _ -> Lwt.return @@ acc
				with
				| Not_found -> Lwt.return @@ (New (match lw with None -> 1 | Some x -> x), None, fn, ln, mdx_id, e)::acc
			)
		)
	) [] c in
	let%lwt act_list = 
		if group = "" then
			Hashtbl.fold (fun uid () acc ->
			let%lwt acc' = acc in
			let%lwt mdx_id = get_student_id uid in 
			let%lwt u = Os_user_proxy.get_data uid in
			let%lwt e = Os_db.User.email_of_userid uid in
			Lwt.return @@ (Deactivate (match lw with None -> 1 | Some x -> x), Some uid, u.fn, u.ln, mdx_id, match e with None -> "" | Some x -> x)::acc')
			rem_hashtbl (Lwt.return act_list0)
		else
			Lwt.return act_list0 in
	let%lwt () = Csv_lwt.close_in c in
	Lwt.return act_list

let%client read_students_csv_file =
	~%(Eliom_client.server_function [%derive.json: string * string * string * int option]
		read_students_csv_file)

let%server filename_of f =
	Eliom_request_info.get_tmp_filename f

let%client filename_of f =
	Js_of_ocaml.Js.to_string f##.name
 
let%shared do_add_students myid () (ayear_v, (group, csv)) =
	let%lwt lw = Moab_terms.learning_week_of_date ayear_v (Date.today ()) in
	let%lwt act_list = read_students_csv_file (filename_of csv, ayear_v, group, lw) in
	Moab_container.page (Some myid) 
	[
		div ~a:[a_class ["content-box"]]
		[
			h1 [txt [%i18n S.changes_to_be_made]];
			Form.post_form ~service:add_students_action2 (fun (ayear, changes_list) ->
				[Form.input ~input_type:`Hidden ~name:ayear ~value:ayear_v Form.string;
				table (
					tr [th []; th [txt [%i18n S.action]]; th [txt [%i18n S.name]]; th [txt [%i18n S.student_id]]; th [txt [%i18n S.email_address]]]::
					changes_list.it (fun (do_b, (act, (uid, (fn, (ln, (mdx_id, email)))))) (act_v, uid_ov, fn_v, ln_v, mdx_id_v, email_v) init ->
						tr [
							td [Form.bool_checkbox_one ~checked:true ~name:do_b ()];
							td (match act_v with
							| New x -> [
									Form.input ~input_type:`Hidden ~name:act ~value:(Printf.sprintf "join_%d" x) Form.string;
									txt [%i18n S.joining_week]; txt " "; txt (string_of_int x)
								]
							| To_group x -> (
									txt [%i18n S.to_group]::txt " "::txt (string_of_int x)::
									Form.input ~input_type:`Hidden ~name:act ~value:(Printf.sprintf "group_%d" x) Form.string::
									(match uid_ov with
									| None -> []
									| Some u -> [Form.input ~input_type:`Hidden ~name:uid ~value:u Form.int64])
								)
							| Deactivate x -> (
									txt [%i18n S.deactivate]::
									Form.input ~input_type:`Hidden ~name:act ~value:(Printf.sprintf "deactivate_%d" x) Form.string::
									(match uid_ov with
									| None -> []
									| Some u -> [Form.input ~input_type:`Hidden ~name:uid ~value:u Form.int64])
								)
							);
							td [txt fn_v; txt " "; txt ln_v;
								Form.input ~input_type:`Hidden ~name:fn ~value:fn_v Form.string;
								Form.input ~input_type:`Hidden ~name:ln ~value:ln_v Form.string	
							];
							td [txt mdx_id_v;
								Form.input ~input_type:`Hidden ~name:mdx_id ~value:mdx_id_v Form.string
							];
							td [txt email_v;
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

let%shared add_students_handler myid () () =
	Moab_base.App.register ~service:add_students_action (Moab_page.connected_page do_add_students);
	Eliom_registration.Any.register ~service:add_students_action2 (Os_session.connected_fun do_add_students2);
	let%lwt student_form = Form.lwt_post_form ~service:add_students_action (fun (ayear, (group, csv)) ->
		let%lwt ayear_widget = Moab_terms.academic_year_select_widget (`Param ayear) in
		Lwt.return [
			table
			[
				tr
				[
					th [txt [%i18n S.academic_year]];
					td [ayear_widget]
				];
				tr
				[
					th [txt [%i18n S.group_number]];
					td [Form.input ~name:group ~input_type:`Text Form.string]
				];
				tr 
				[
					th [txt [%i18n S.csv_file_from_misis]];
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
			h1 [txt [%i18n S.add_students ~capitalize:true]];
			student_form
		]
	]
