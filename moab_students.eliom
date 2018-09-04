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

let%server do_add_students myid () (term, (group, csv)) =
	Ocsigen_messages.console (fun () -> Printf.sprintf "[do_add_students] t: %s g: %s csv: %s" term group csv.Ocsigen_extensions.tmp_filename);
	let%lwt f = Lwt_io.open_file ~mode:Lwt_io.Input csv.Ocsigen_extensions.tmp_filename in
	let%lwt c = Csv_lwt.of_channel f in
	let%lwt rows = Csv_lwt.fold_left (fun acc l ->
		let _::name::id::_::_::_::_::_::_::_::mail::[] = l in
		Scanf.sscanf name "%s@, %s" (fun ln fn ->
			let name = Printf.sprintf "%s %s" fn ln in
			Scanf.sscanf mail "mailto:%s" (fun e ->
				try%lwt
					let%lwt id = Moab_user.find_user e in
					Lwt.return @@ tr [td [pcdata name]; td [pcdata (Printf.sprintf "userid %Ld" id)]]::acc
				with
				| Not_found -> Lwt.return @@ tr [td [pcdata name]; td [pcdata "new user"]]::acc
			)
		)
	) [] c in
	let%lwt () = Csv_lwt.close_in c in
	Moab_container.page (Some myid) 
	[
		div ~a:[a_class ["content-box"]]
		[
			h1 [pcdata "Changes to be made"];
			table (
				tr [th [pcdata "Name"]; th [pcdata "Action"]]::
				rows
			)	
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
			h1 [pcdata "Adding students"];
			Form.post_form ~service:add_students_action (fun (term, (group, csv)) ->
			[
				table
				[
					tr
					[
						th [pcdata "Term"];
						td [match terms with
						| [] -> pcdata "no terms set up yet"
						| h::t -> Form.select ~name:term Form.string (term_opt h) (List.map term_opt t)
						]
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
						td ~a:[a_colspan 2] [Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:"Save" Form.string]
					]
				]
			]) ()
		]
	]

let%server add_students_handler myid () () =
	Moab_base.App.register ~scope:Eliom_common.default_session_scope
		~service:add_students_action (Moab_page.connected_page do_add_students);
	real_add_students_handler myid () ()

let%client add_students_handler =
	real_add_students_handler
