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

let grade_table_page () () =
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, _, _, is_admin) -> 
		Lwt.catch (fun () ->
			container
			[
				h1 [pcdata "Grade table"];
			]
		)
		(function
		| Moab_db.No_group -> error_page "You are an administrator."
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Eliom_registration.Any.register ~service:grade_table_service grade_table_page;
;;
