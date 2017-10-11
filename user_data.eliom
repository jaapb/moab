[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
]

[%%server
	open Services
	open Moab
	open CalendarLib
]

let user_data_err = Eliom_reference.eref ~scope:Eliom_common.request_scope None;;

let do_user_data_page () (fname, (lname, (password, password2))) =
	Lwt.catch (fun () ->
		let%lwt x = Eliom_reference.get user in
		match x with
		| None -> 
				let%lwt () = Eliom_reference.set user_data_err (Some "Could not find user (this should not happen)") in
				Eliom_registration.Action.send ()
		| Some (user_id, _, _, is_admin) ->
				if password <> "" && password <> password2 then
					let%lwt () = Eliom_reference.set user_data_err (Some "Passwords are not identical") in
						Eliom_registration.Action.send ()
				else 
					let%lwt () = Eliom_reference.set user (Some (user_id, fname, lname, is_admin)) in
					let%lwt () = Moab_db.set_user_data user_id fname lname password in
					Eliom_registration.Redirection.send (Eliom_registration.Redirection main_service)
	)
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let user_data_page () () =
	let do_user_data_service = create_attached_post ~fallback:user_data_service
		~post_params:(string "first_name" ** string "last_name" ** string "password" ** string "password2") () in
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:do_user_data_service do_user_data_page;
	let%lwt u = Eliom_reference.get user in
	let%lwt err = Eliom_reference.get user_data_err in
	let%lwt () = Eliom_reference.set user_data_err None in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, fname, lname, _) -> Lwt.catch (fun () ->
			container
			[
				h1 [pcdata "Your data"];
				p [match err with
				| None -> pcdata "You can change your name and password here."
				| Some e -> pcdata e];
				Form.post_form ~service:do_user_data_service (fun (first_name, (last_name, (password, password2))) ->
				[
					table
					[
						tr [
							th [b [pcdata "First name: "]];
							td [Form.input ~input_type:`Text ~name:first_name ~value:fname Form.string]
						];
						tr [
							th [b [pcdata "Last name: "]];
							td [Form.input ~input_type:`Text ~name:last_name ~value:lname Form.string]
						];
						tr [
							th [b [pcdata "Password: "]];
							td [Form.input ~input_type:`Password ~name:password Form.string]
						];
						tr [
							th [b [pcdata "Confirm password: "]];
							td [Form.input ~input_type:`Password ~name:password2 Form.string]
						];
						tr [
							td ~a:[a_colspan 2] [
								Form.input ~input_type:`Submit ~value:"Save" Form.string
							]
						]
					]
				]) ()
			]
		)
		(function
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Eliom_registration.Any.register ~service:user_data_service user_data_page;
;;
