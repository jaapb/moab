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

let do_approve_blog_page (user_id, lw) () =
	Lwt.catch (fun () ->
		let%lwt () = Moab_db.approve_blog !Moab.term user_id lw in
		Eliom_registration.Redirection.send (Eliom_registration.Redirection main_service)
	)
	(function 
	| e -> error_page (Printexc.to_string e)
	)

let view_blog_page (user_id, lw) () =
	let do_approve_blog_service = create_attached_post ~fallback:view_blog_service
		~post_params:unit () in
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope 
		~service:do_approve_blog_service do_approve_blog_page;
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, _, _, is_admin) -> 
		Lwt.catch (fun () ->
			if (not is_admin) && user_id <> uid then
				error_page "You cannot access someone else's blog."
			else
				let%lwt (title, blog) = Moab_db.get_blog user_id lw !Moab.term in
				container
				(
					h1 [pcdata title]::
					pre [pcdata blog]::
					(if is_admin then
						[h2 [pcdata "Actions"];
						Form.post_form ~service:do_approve_blog_service (fun () ->
							[Form.input ~input_type:`Submit ~value:"Approve blog" Form.string]
						) (user_id, lw)
						]
					else
						[]
					)
				)
		)
		(function
		| Moab_db.No_group -> error_page "you are an administrator"
		| Not_found -> error_page "This blog does not exist"
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Eliom_registration.Any.register ~service:view_blog_service view_blog_page;
;;
