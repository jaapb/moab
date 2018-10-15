[%%shared
	open Lwt.Infix
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

(* Local services *)

let%server edit_blog_action = Eliom_service.create_attached_post
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

(* Utility functions *)

let%shared blog_score uid =
	let ayear = ~%(!Moab_config.current_academic_year) in
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
	let ayear = ~%(!Moab_config.current_academic_year) in
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
					| None -> ["dt-bad"]
					| Some _ -> ["dt-good"]
				end
		in
		Lwt.return @@	td ~a:[a_class att_class] [match x with
		| None -> pcdata (string_of_int week_nr)
		| Some w -> a ~service:Moab_services.show_blog_service [pcdata (string_of_int week_nr)] (Some uid, Some week_nr)
		]
	) weeks in
	let%lwt pred_score = blog_score uid in
	Lwt.return @@ tr (
		td [b [pcdata [%i18n S.your_blogs]]; pcdata " "]::
		(week_list @ [td [b [pcdata (string_of_int pred_score)]]])
	)

let%shared blog_report () =
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt blogs = get_approvable_blogs ayear in
	let%lwt trs = Lwt_list.map_s (fun (uid, title, week) ->
		let%lwt (fn, ln) = Moab_users.get_name uid in
		Lwt.return @@ tr [
			td [pcdata fn; pcdata " "; pcdata ln];
			td [pcdata title];
			td [pcdata (string_of_int week)]
		]) blogs in
	Lwt.return @@ table (trs)

(* Handlers *)

let%shared show_blog_handler myid (opt_uid, opt_week) () =
	let uid = match opt_uid with
	| None -> myid
	| Some x -> x in
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let display_blog week =
		let%lwt x = get_blog_opt (uid, ayear, week) in
		match x with
		| None -> Lwt.return @@ p [pcdata [%i18n S.no_blog_for_week]]
		| Some (title, text) -> Lwt.return @@
			div ~a:[a_class ["content-box"]]
			(
				h1 [pcdata title]::
				List.map (fun x ->
					p [pcdata x]
				) (List.filter (fun x -> x <> "") (String.split_on_char '\n' text))
			)
		in
	let%lwt blog = match opt_week, lw with
	| None, None -> Lwt.return @@ p [pcdata [%i18n S.no_week_specified]] 
	| Some w, _ -> display_blog w
	| _, Some w -> display_blog w in
	Moab_container.page (Some myid) 
	[
		blog
	]

let do_edit_blog myid () (title, text) =
	let ayear = !Moab_config.current_academic_year in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let%lwt () =
		match lw with
		| None ->
			Os_msg.msg ~level:`Msg ~onload:true [%i18n S.no_blog_needed];
			Lwt.return_unit
		| Some learning_week ->
			update_blog (myid, ayear, learning_week, title, text) in
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared real_edit_blog_handler myid () () =
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let%lwt blog_content = match lw with
	| None -> Lwt.return [p [pcdata [%i18n S.no_blog_needed]]]
	| Some learning_week -> 
		let%lwt x = get_blog_opt (myid, ayear, learning_week) in
		let (title_v, text_v) = match x with
		| None -> ("", "")
		| Some (tt, tx) -> (tt, tx) in
		Lwt.return [
			p [pcdata [%i18n S.writing_blog_for_week]; pcdata " "; pcdata (string_of_int learning_week)];
			p [pcdata [%i18n S.blog_message]];
			Form.post_form ~service:edit_blog_action (fun (title, text) -> [
				table [
					tr [
						th [pcdata [%i18n S.title]];
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
			h1 [pcdata [%i18n S.write_blog]]::
			blog_content
		)
	]

let%server edit_blog_handler myid () () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:edit_blog_action (Os_session.connected_fun do_edit_blog);
	real_edit_blog_handler myid () ()

let%client edit_blog_handler =
	real_edit_blog_handler
