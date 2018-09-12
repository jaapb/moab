[%%shared
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

let%server set_student_info (uid, term, mdx_id, joined_week, left_week) =
	Moab_student_db.set_student_info uid term mdx_id joined_week left_week

let%client set_student_info =
	~%(Eliom_client.server_function [%derive.json : int64 * string * string * int * int option]
		(Os_session.connected_wrapper set_student_info))

let%server add_students_action =
	Eliom_service.create_attached_post
		~fallback:Moab_services.add_students_service
		~post_params:(string "term" ** string "group" ** file "csv")
		()

let%client add_students_action = 
	~%add_students_action

let%server add_students_action2 =
	Eliom_service.create_attached_post
		~fallback:Moab_services.add_students_service
		~post_params:(string "term" ** list "changes" (bool "do" ** string "first_name" ** string "last_name" ** string "mdx_id" ** string "email"))
		()

let%client add_students_action2 = 
	~%add_students_action2

let%server do_add_students2 myid () (term, changes_list) =
	Ocsigen_messages.console (fun () -> Printf.sprintf "[do_add_students2] new: %d" (List.length changes_list));
	let%lwt lwo = Moab_terms.learning_week_of_date term (Date.today ()) in
	let lw = match lwo with
		| None -> 1
		| Some x -> x in
	let%lwt () = Lwt_list.iter_s (fun (do_b, (fn, (ln, (mdx_id, email)))) ->
		if do_b then
			let%lwt uid = Moab_users.add_user (Student, fn, ln, email, Some mdx_id) in
			let%lwt () = set_student_info (uid, term, mdx_id, lw, None) in
			Lwt.return_unit	
		else
			Lwt.return_unit
	) changes_list	in
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)
			
let%server do_add_students myid () (term_v, (group, csv)) =
	Ocsigen_messages.console (fun () -> Printf.sprintf "[do_add_students] t: %s g: %s csv: %s" term_v group csv.Ocsigen_extensions.tmp_filename);
	let%lwt lwo = Moab_terms.learning_week_of_date term_v (Date.today ()) in
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
					Lwt.return @@ acc 
				with
				| Not_found -> Lwt.return @@ (`New lw, fn, ln, mdx_id, e)::acc
			)
		)
	) [] c in
	let%lwt () = Csv_lwt.close_in c in
	Moab_container.page (Some myid) 
	[
		div ~a:[a_class ["content-box"]]
		[
			h1 [pcdata [%i18n S.changes_to_be_made]];
			Form.post_form ~service:add_students_action2 (fun (term, changes_list) ->
				[Form.input ~input_type:`Hidden ~name:term ~value:term_v Form.string;
				table (
					tr [th []; th [pcdata [%i18n S.action]]; th [pcdata [%i18n S.name]]; th [pcdata [%i18n S.student_id]]; th [pcdata [%i18n S.email_address]]]::
					changes_list.it (fun (do_b, (fn, (ln, (mdx_id, email)))) (act_v, fn_v, ln_v, mdx_id_v, email_v) init ->
						tr [
							td [Form.bool_checkbox_one ~checked:true ~name:do_b ()];
							td (match act_v with
							| `New x -> [pcdata [%i18n S.joining_week]; pcdata " "; pcdata (string_of_int lw)]
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
						tr [td ~a:[a_colspan 2] [Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:"Save" Form.string]]
					]
				)]
			) ()
		]
	]

let%shared real_add_students_handler myid () () =
	let term_opt t =
		Form.Option ([], t, None, false) in
	let%lwt terms = Moab_terms.get_terms () in
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]]
		[
			h1 [pcdata [%i18n S.add_students]];
			Form.post_form ~service:add_students_action (fun (term, (group, csv)) ->
			[
				table
				[
					tr
					[
						th [pcdata [%i18n S.term]];
						td [match terms with
						| [] -> pcdata [%i18n S.no_terms_yet]
						| h::t -> Form.select ~name:term Form.string (term_opt h) (List.map term_opt t)
						]
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
						td ~a:[a_colspan 2] [Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:"Save" Form.string]
					]
				]
			]) ()
		]
	]

let%server add_students_handler myid () () =
	Moab_base.App.register ~scope:Eliom_common.default_session_scope
		~service:add_students_action (Moab_page.connected_page do_add_students);
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:add_students_action2 (Os_session.connected_fun do_add_students2);
	real_add_students_handler myid () ()

let%client add_students_handler =
	real_add_students_handler
