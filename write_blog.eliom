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

let rec find_nr f l s =
	match l with
	| [] -> raise Not_found
	| h::t -> if f h then s else find_nr f t (s+1)
;;

let do_write_blog_page () (user_id, (week, (year, (title, text)))) =
	Moab_db.update_blog user_id week year title text >>=
	fun () ->
	container [
		h1 [pcdata "Blog saved"];
		p [pcdata (Printf.sprintf "Your entry for week %d has been saved. " week); pcdata "You will be able to update it until the end of the week."]
	]
;;

let write_blog_page () () =
	let do_write_blog_service = create_attached_post ~fallback:write_blog_service
		~post_params:(string "user_id" ** int "week" ** int "year" ** string "title" ** string "text") () in
		Moab_app.register ~scope:Eliom_common.default_session_scope
			~service:do_write_blog_service do_write_blog_page;
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> container []
	| Some (uid, _, _) -> 
		Lwt.catch (fun () ->
			let now = Date.today () in
			let week = Date.week now in
			let year = Date.year now in
			let%lwt group = Moab_db.get_user_group uid in
			let%lwt lws = Moab_db.get_learning_weeks group !Moab.term in
			let this_lw = find_nr (fun (w, y) -> Int32.to_int w = week && y = year) lws 1 in
			let%lwt (v_title, v_text) = Lwt.catch
				(fun () -> Moab_db.get_blog uid week year)
				(function
				| Not_found -> Lwt.return ("", "")
				| e -> Lwt.fail e) in
			container
			[ 
				h1 [pcdata "Blog"];
				p [pcdata (Printf.sprintf "You are now writing your blog for week %d" this_lw)];
				p [pcdata "You are welcome to use any writing style you like, but one suggestion is to use Scrum-style stand-ups and answer the following questions:"];
				ol [
					li [pcdata "What did I do last week and how did it help my project progess?"];
					li [pcdata "What will I do next week and how will it help my project progress?"];
					li [pcdata "What obstacles can I see to successfully complete my project?"]
				];
				Form.post_form ~service:do_write_blog_service
				(fun (user_id, (wk, (yr, (title, text)))) -> [
					Form.input ~input_type:`Hidden ~name:user_id ~value:uid Form.string;
					Form.input ~input_type:`Hidden ~name:wk ~value:week Form.int;
					Form.input ~input_type:`Hidden ~name:yr ~value:year Form.int; 
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
		| Not_found -> container [
				h1 [pcdata "Not a learning week"];
				p [pcdata "This week is not a learning week, so no need to write a blog for this week."]
			]
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Moab_app.register ~service:write_blog_service write_blog_page;
;;
