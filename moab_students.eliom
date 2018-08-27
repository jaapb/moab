[%%shared
	open Eliom_content.Html.F
	open Eliom_parameter
]

let%server add_students_action =
	Eliom_service.create_attached_post
		~fallback:Moab_services.add_students_service
		~post_params:(string "term" ** string "group" ** file "csv")
		()

let%client add_students_action =
	~%add_students_action

let%server do_add_students () (term, (group, csv)) =
	Ocsigen_messages.console (fun () -> Printf.sprintf "[do_add_students] t: %s g: %s csv: %s" term group csv.Ocsigen_extensions.tmp_filename);
	let%lwt f = Lwt_io.open_file ~mode:Lwt_io.Input csv.Ocsigen_extensions.tmp_filename in
	let%lwt c = Csv_lwt.of_channel f in
	let%lwt () = Csv_lwt.iter (fun l ->
		let _::name::id::_ = l in
		Scanf.sscanf name "%s@, %s" (fun ln fn ->
			Ocsigen_messages.console (fun () -> Printf.sprintf "  - id: %s name: %s %s" id fn ln);
			Moab_user.add_student (fn, ln, id)
		);
		Lwt.return_unit
	) c in
	let%lwt () = Csv_lwt.close_in c in
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared real_add_students_handler myid () () =
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]]
		[
			h1 [pcdata "Adding students"];
			Form.post_form ~service:add_students_action (fun (term, (group, csv)) ->
			[
				table
				[
					tr
					[
						th [pcdata "Term (give first year)"];
						td [Form.input ~name:term ~input_type:`Text Form.string]
					];
					tr
					[
						th [pcdata "Group number"];
						td [Form.input ~name:group ~input_type:`Text Form.string]
					];
					tr 
					[
						th [pcdata "CSV file from MISIS"];
						td [Form.file_input ~name:csv ()]
					];
					tr
					[ 
						td ~a:[a_colspan 2; a_class ["button"]] [Form.input ~input_type:`Submit ~value:"Save" Form.string]
					]
				]
			]) ()
		]
	]

let%server add_students_handler myid () () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:add_students_action do_add_students;
	real_add_students_handler myid () ()

let%client add_students_handler =
	real_add_students_handler
