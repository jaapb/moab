[%%shared
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
			Form.post_form ~service:edit_blog_action (fun (title, text) -> [
				table [
					tr [
						th [pcdata [%i18n S.title]];
						td [Form.input ~input_type:`Text ~name:title ~value:title_v Form.string]
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
