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

let do_write_blog_page () (user_id, (week, (year, (title, text)))) =
	Lwt.catch (fun () ->
		let%lwt () = Moab_db.update_blog user_id week year title text in
		container [
			h1 [pcdata "Blog saved"];
			p [pcdata (Printf.sprintf "Your entry for week %d has been saved. " week); pcdata "You will be able to update it until the end of the week."]
		]
	)
	(function
	| e -> error_page (Printexc.to_string e)
	)
;;

let write_blog_page () () =
	let do_write_blog_service = create_attached_post ~fallback:write_blog_service
		~post_params:(string "user_id" ** int "week" ** int "year" ** string "title" ** string "text") () in
		Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
			~service:do_write_blog_service do_write_blog_page;
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, _, _, _) -> 
		Lwt.catch (fun () ->
			let term = !Moab.term in
			let%lwt (group, _, _) = Moab_db.get_user_group uid term in
			let%lwt this_lw = Moab_db.current_learning_week group term in
			match this_lw with
			| None -> container [
					h1 [pcdata "Not a learning week"];
					p [pcdata "This week is not a learning week, so no need to write a blog for this week."]
				]
			| Some lw -> let%lwt (v_title, v_text) = Lwt.catch
				(fun () -> Moab_db.get_blog uid lw term)
				(function
				| Not_found -> Lwt.return ("", "")
				| e -> Lwt.fail e) in
				container
				[ 
					h1 [pcdata "Blog"];
					p [pcdata (Printf.sprintf "You are now writing your blog for week %d" lw)];
					p [em [pcdata "Please note that this blog is about your project, not about what you learned this week in the module or about your presentation."]];
					p [pcdata "You are welcome to use any writing style you like, but one suggestion is to use Scrum-style stand-ups and answer the following questions:"];
					ol [
						li [pcdata "What did I do last week and how did it help my project progess?"];
						li [pcdata "What will I do next week and how will it help my project progress?"];
						li [pcdata "What obstacles can I see to successfully complete my project?"]
					];
					Form.post_form ~service:do_write_blog_service
					(fun (user_id, (wk, (yr, (title, text)))) -> [
						Form.input ~input_type:`Hidden ~name:user_id ~value:uid Form.string;
						Form.input ~input_type:`Hidden ~name:wk ~value:lw Form.int;
						Form.input ~input_type:`Hidden ~name:yr ~value:(Date.year (Date.today ())) Form.int; 
						table [
							tr [
								td [pcdata "Title: "];
								td [Form.input ~input_type:`Text ~name:title ~value:v_title Form.string]
							];
							tr [
								td ~a:[a_colspan 2] [
									Form.textarea ~a:[a_rows 20; a_cols 60] ~name:text ~value:v_text ()
								]
							];
							tr [
								td ~a:[a_colspan 2] [Form.input ~input_type:`Submit ~value:"Save" Form.string]
							]
						]
					]) ()
				]
		)
		(function
		| Moab_db.No_group -> error_page "you are an administrator"
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Eliom_registration.Any.register ~service:write_blog_service write_blog_page;
;;
