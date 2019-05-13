[%%shared
	open Lwt.Infix
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

(* Local services *)

let%server edit_blog_action = Eliom_service.create_attached_post
	~name:"edit_blog_action"
	~fallback:Moab_services.edit_blog_service
	~post_params:(string "title" ** string "text")
	()

let%client edit_blog_action = 
	~%edit_blog_action

(* Database access *)

let%server get_blog (uid, ayear, lw) =
	Moab_blog_db.get_blog uid ayear lw

let%client get_blog =
	~%(Eliom_client.server_function [%derive.json: int64 * string * int]
		(Os_session.connected_wrapper get_blog))

let%server get_blog_opt (uid, ayear, lw) =
	try%lwt
		let%lwt x = get_blog (uid, ayear, lw) in
		Lwt.return_some x
	with
		Not_found -> Lwt.return_none

let%client get_blog_opt =
	~%(Eliom_client.server_function [%derive.json: int64 * string * int]
		(Os_session.connected_wrapper get_blog_opt))

let%server update_blog (uid, ayear, lw, title, text) =
	Moab_blog_db.update_blog uid ayear lw title text

let%client update_blog =
	~%(Eliom_client.server_function [%derive.json: int64 * string * int * string * string]
		(Os_session.connected_wrapper update_blog))

let%server get_nr_blogs (uid, ayear, approved_only) =
	Moab_blog_db.get_nr_blogs uid ayear approved_only

let%client get_nr_blogs =
	~%(Eliom_client.server_function [%derive.json: int64 * string * bool]
		(Os_session.connected_wrapper get_nr_blogs))

let%server get_approvable_blogs ayear =
	Moab_blog_db.get_approvable_blogs ayear

let%client get_approvable_blogs =
	~%(Eliom_client.server_function [%derive.json: string]
		(Os_session.connected_wrapper get_approvable_blogs))

let%server approve_blog (ayear, uid, week) =
	Moab_blog_db.set_blog_status ayear uid week true

let%client approve_blog =
	~%(Eliom_client.server_function [%derive.json: string * int64 * int]
		(Os_session.connected_wrapper approve_blog))

let%server disapprove_blog (ayear, uid, week) =
	Moab_blog_db.set_blog_status ayear uid week false

let%client disapprove_blog =
	~%(Eliom_client.server_function [%derive.json: string * int64 * int]
		(Os_session.connected_wrapper disapprove_blog))

(* Utility functions *)

let%shared blog_score uid =
	let ayear = !(~%Moab_config.current_academic_year) in
	let%lwt weeks = Moab_terms.get_learning_weeks ayear in
	let year = Date.year (Date.today ()) in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in 
	let%lwt (jw, lfw0) = Moab_students.get_active_period (ayear, uid) in
	let lfw = match lfw0 with None -> List.length weeks | Some x -> x in
	let learning_week = match lw with None -> 0 | Some x -> x in
	let%lwt b = get_nr_blogs (uid, ayear, true) >|= Int64.to_int in
	let period = lfw - jw + 1 in
	let pred_score = 
		if learning_week = 0
		then 0
		else max 0 ((period * b / (learning_week - jw + 1)) - period + 10) in
	Lwt.return pred_score

let%shared blog_tr uid =
	let ayear = !(~%Moab_config.current_academic_year) in
	let%lwt weeks = Moab_terms.get_learning_weeks ayear in
	let year = Date.year (Date.today ()) in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in 
	let%lwt (jw, lfw0) = Moab_students.get_active_period (ayear, uid) in
	let lfw = match lfw0 with None -> List.length weeks + 1 | Some x -> x in
	let%lwt week_list = Lwt_list.mapi_s (fun i (_, w, y) ->
		let week_nr = i + 1 in
		let%lwt x = get_blog_opt (uid, ayear, week_nr) in
		let att_class = 
			match lw with
			| None -> []
			| Some learning_week ->
				if week_nr > learning_week then []
				else if week_nr < jw then []
				else if week_nr > lfw then []
				else begin
					match x with
					| Some (_, _, Some true) -> ["dt-good"]
					| Some (_, _, None) -> ["dt-warning"]
					| _ -> ["dt-bad"]
				end
		in
		Lwt.return @@	td ~a:[a_class att_class] [match x with
		| None -> txt (string_of_int week_nr)
		| Some w -> a ~service:Moab_services.show_blog_service [txt (string_of_int week_nr)] (Some uid, Some week_nr)
		]
	) weeks in
	let%lwt pred_score = blog_score uid in
	Lwt.return @@ tr (
		td [b [txt [%i18n S.your_blogs]]; txt " "]::week_list
		(* (week_list @ [td [b [txt (string_of_int pred_score)]]]) *)
	)

let%shared blog_report () =
	let ayear = !(~%Moab_config.current_academic_year) in
	let%lwt blogs = get_approvable_blogs ayear in
	let%lwt trs = Lwt_list.map_s (fun (uid, title, week) ->
		let%lwt (fn, ln) = Moab_users.get_name uid in
		Lwt.return @@ tr [
			td [txt fn; txt " "; txt ln];
			td [a ~service:Moab_services.show_blog_service [txt title] (Some uid, Some week)];
			td [txt (string_of_int week)]
		]) blogs in
	Lwt.return @@ table (trs)

(* Handlers *)

let%shared show_blog_handler myid (opt_uid, opt_week) () =
	let uid = match opt_uid with
	| None -> myid
	| Some x -> x in
	let%lwt tp = Moab_users.get_user_type myid in
	let%lwt u = Os_user_proxy.get_data  uid in
	let ayear = !(~%Moab_config.current_academic_year) in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let approve_button = D.button ~a:[a_class ["button"; "approve"]] [txt [%i18n S.approve]] in
	let disapprove_button = D.button ~a:[a_class ["button"; "disapprove"]] [txt [%i18n S.disapprove]] in
	let display_blog week =
		let%lwt x = get_blog_opt (uid, ayear, week) in
		match x with
		| None -> Lwt.return @@ p [txt [%i18n S.no_blog_for_week]]
		| Some (title, text, _)  -> Lwt.return @@
			div ~a:[a_class ["content-box"]]
			(List.flatten [
				[h1 [txt title]];
				(if myid <> uid then [i [txt "By "; txt u.fn; txt " "; txt u.ln]] else []);	
				List.map (fun x ->
					p [txt x]
				) (List.filter (fun x -> x <> "") (String.split_on_char '\n' text));
				(if tp = Admin then [approve_button; disapprove_button] else [])
			])
		in
	let%lwt blog = match opt_week, lw with
	| None, None -> Lwt.return @@ p [txt [%i18n S.no_week_specified]] 
	| Some w, _ -> display_blog w
	| _, Some w -> display_blog w in
	ignore [%client ((Lwt.async @@ fun () ->
		let button = Eliom_content.Html.To_dom.of_button ~%approve_button in
		Lwt_js_events.clicks button @@ fun _ _ ->
			match ~%opt_week with
			| None -> let%lwt _ =
					Ot_popup.popup ~close_button:[Os_icons.F.close ()] (fun _ -> Lwt.return @@ p [txt [%i18n S.no_week_specified]]) in
				Lwt.return_unit
			| Some w ->
				let ay = ~%ayear in
				approve_blog (ay, ~%uid, w);
				let%lwt l = get_approvable_blogs ay in
				(match l with
				| [] ->	Eliom_client.change_page ~service:Os_services.main_service () ()
				| (u, _, w)::_ -> Eliom_client.change_page ~service:Moab_services.show_blog_service (Some u, Some w) ())
	): unit)];
	ignore [%client ((Lwt.async @@ fun () ->
		let button = Eliom_content.Html.To_dom.of_button ~%disapprove_button in
		Lwt_js_events.clicks button @@ fun _ _ ->
			match ~%opt_week with
			| None -> let%lwt _ =
					Ot_popup.popup ~close_button:[Os_icons.F.close ()] (fun _ -> Lwt.return @@ p [txt [%i18n S.no_week_specified]]) in
				Lwt.return_unit
			| Some w ->
				let ay = ~%ayear in
				disapprove_blog (ay, ~%uid, w);
				let%lwt l = get_approvable_blogs ay in
				(match l with
				| [] ->	Eliom_client.change_page ~service:Os_services.main_service () ()
				| (u, _, w)::_ -> Eliom_client.change_page ~service:Moab_services.show_blog_service (Some u, Some w) ())
	): unit)];
	Moab_container.page (Some myid) 
	[
		blog
	]

let%shared do_edit_blog myid () (title, text) =
	let ayear = !(~%Moab_config.current_academic_year) in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let%lwt d =
		match lw with
		| None ->
			Lwt.return @@
				div ~a:[a_class ["content-box"]] [
					h1 [txt [%i18n S.error ~capitalize:true]];
					p [txt [%i18n S.no_blog_needed]]
				]
		| Some learning_week ->
			let%lwt () = update_blog (myid, ayear, learning_week, title, text) in
			Lwt.return @@
				div ~a:[a_class ["content-box"]] [
					h1 [txt [%i18n S.success]];
					p [txt [%i18n S.blog_stored]]
				] in
	Moab_container.page (Some myid) [d]

let%shared edit_blog_handler myid () () =
	Moab_base.App.register ~service:edit_blog_action (Moab_page.connected_page do_edit_blog);
	let ayear = !(~%Moab_config.current_academic_year) in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let%lwt blog_content = match lw with
	| None -> Lwt.return [p [txt [%i18n S.no_blog_needed]]]
	| Some learning_week -> 
		let%lwt x = get_blog_opt (myid, ayear, learning_week) in
		let (title_v, text_v) = match x with
		| None -> ("", "")
		| Some (tt, tx, _) -> (tt, tx) in
		Lwt.return [
			p [txt [%i18n S.writing_blog_for_week]; txt " "; txt (string_of_int learning_week)];
			p [b [txt [%i18n S.blog_message1]]; txt " "; txt [%i18n S.blog_message2]];
			p ~a:[a_class ["warning-paragraph"]] [txt [%i18n S.blog_message3]];
			Form.post_form ~service:edit_blog_action (fun (title, text) -> [
				table [
					tr [
						th [txt [%i18n S.title]];
						td [Form.input ~a:[a_size 60] ~input_type:`Text ~name:title ~value:title_v Form.string]
					];
					tr [
						td ~a:[a_colspan 2] [
							Form.textarea ~a:[a_rows 20; a_cols 60] ~name:text ~value:text_v ()
						]
					];
					tr [
						td ~a:[a_colspan 2] [
							Form.input ~input_type:`Submit ~a:[a_class ["button"]] ~value:[%i18n S.save]
								Form.string
						]
					]
				]
			]) ()
		] in
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]] (
			h1 [txt [%i18n S.write_blog]]::
			blog_content
		)
	]
