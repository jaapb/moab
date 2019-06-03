[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
]

[%%client
	open Js_of_ocaml
]

(* Local services *)

let%server submit_report_action = Eliom_service.create_attached_post
	~name:"submit_report_action"
	~fallback:Moab_services.submit_report_service
	~post_params:(any)
	()

let%server report_upload_service = Eliom_registration.Ocaml.create
	~name:"report_upload_service"
	~path:Eliom_service.No_path
	~meth:(Eliom_service.Post (unit, file "f"))
	(fun () f -> Lwt.return_unit)

let%server report_feedback_action = Eliom_service.create_attached_post
	~name:"report_feedback_action"
	~fallback:Moab_services.report_feedback_service
	~post_params:(int64 "student_id" ** string "quality_feedback" ** int "quality_grade" ** string "independence_feedback" ** int "independence_feedback" ** string "community_feedback" ** int "community_grade")
	()

let%client submit_report_action = 
	~%submit_report_action

let%client report_upload_service =
	~%report_upload_service

let%client report_feedback_action =
	~%report_feedback_action

(* Database access *)

let%server set_report_feedback (ayear, gnr, qf, qg, inf, ing, cf, cg) =
	Moab_report_db.set_report_feedback ayear gnr qf qg inf ing cf cg

let%client set_report_feedback =
	~%(Eliom_client.server_function [%derive.json: string * int64 * string * int * string * int * string * int]
		(Os_session.connected_wrapper set_report_feedback))

let%server get_report_feedback (ayear, gnr) =
	Moab_report_db.get_report_feedback ayear gnr

let%client get_report_feedback =
	~%(Eliom_client.server_function [%derive.json: string * int64]
		(Os_session.connected_wrapper get_report_feedback))

(* Utility functions *)

(* Client side functions *)

let%client handle_add_url input_element rl_handle () =
	let input = Eliom_content.Html.To_dom.of_input input_element in
	Lwt_js_events.changes input @@ fun _ _ ->
	let url = Js.to_string input##.value in
	if (Js.Unsafe.coerce input)##.validity##.valid = Js.bool true then
	begin
		Eliom_shared.ReactiveData.RList.snoc (`URL url) rl_handle;
		input##.value := Js.string ""
	end;
	Lwt.return_unit

let%client handle_add_file input_element rl_handle () =
	let input = Eliom_content.Html.To_dom.of_input input_element in
	Lwt_js_events.changes input @@ fun _ _ ->
	Js.Optdef.case (input##.files)
	Lwt.return
	(fun files ->
		Js.Opt.case (files##(item (0)))
		Lwt.return
		(fun f ->
			Eliom_client.call_ocaml_service
				~service:report_upload_service () f
		)
	)

(* Handlers *)

let%shared do_submit_report myid () params =
	let params_tr = List.map (fun (n, v) ->
		tr [
			td [txt n];
			td [txt v]
		]) params in
	let page = [
		table (tr [th [txt "Name"]; th [txt "Value"]]::params_tr)
	] in	
	Moab_base.App.send
		(Moab_page.make_page (Os_page.content page))
	
let%shared submit_report_handler myid () () =
	Eliom_registration.Any.register ~service:submit_report_action
		(Os_session.connected_fun do_submit_report);

	let draft = D.Raw.input ~a:[a_input_type `Radio; a_name "is_draft"; a_value "true"; a_checked ()] () in
	let final_version = D.Raw.input ~a:[a_input_type `Radio; a_name "is_draft"; a_value "false"] () in
	let quality_url = D.Raw.input ~a:[a_input_type `Url; a_name "quality_url"] () in
	let quality_file =	D.Raw.input ~a:[a_input_type `File; a_name "quality_file"; a_class ["ot-pup-input"]; a_accept ["text/*"]] () in
	let (quality_l, quality_h) = Eliom_shared.ReactiveData.RList.create [] in

	(* client side handlers *)

	(* do not submit on enter *)
	ignore [%client ((Lwt.async @@ fun () ->
		Lwt_js_events.keydowns Dom_html.window @@ fun ev _ ->
		Dom.preventDefault ev;
		Lwt.return_unit
	): unit)];

	(* click final version radio confirmation *)
	ignore [%client ((Lwt.async @@ fun () ->
		let d = Eliom_content.Html.To_dom.of_input ~%draft in
		let fv = Eliom_content.Html.To_dom.of_input ~%final_version in
		Lwt_js_events.clicks ~use_capture:true fv @@ fun _ _ ->
		let%lwt ok = Ot_popup.confirm [p [txt [%i18n S.final_version_warning1]];
			p [txt [%i18n S.final_version_warning2]];
			p [txt [%i18n S.final_version_warning3]]
		]
		[txt [%i18n S.yes]] [txt [%i18n S.no]] in
		if not ok then
		begin
			d##.checked := Js.bool true;
			fv##.checked := Js.bool false
		end;
		Lwt.return_unit
	): unit)];

	(* add evidence URLs *)
	ignore [%client (Lwt.async (handle_add_url ~%quality_url ~%quality_h): unit)];

	(* add evidence file *)
	ignore [%client (Lwt.async (handle_add_file ~%quality_file ~%quality_h): unit)];

	let quality_lis = Eliom_shared.ReactiveData.RList.map
		[%shared ((function
			| `URL url -> li [txt url]
			| _ -> li [txt "summat else"]
		): _ -> _)] quality_l in

	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]] [
			h1 [txt [%i18n S.submit_report]];
			Eliom_content.Html.F.Form.post_form ~service:submit_report_action (fun is_draft ->
			[
				table [
					tr [
						td [label [draft; txt " "; txt [%i18n S.draft]]];
						td [label [final_version; txt " "; txt [%i18n S.final_version]]]
					];
				];
				h2 [txt [%i18n S.introduction]];
				h3 [txt [%i18n S.text]];
				Raw.textarea ~a:[a_cols 80; a_rows 8; a_placeholder [%i18n S.introduction_text]; a_name "introduction_text"] (txt "");
				h2 [txt [%i18n S.quality]];
				h3 [txt [%i18n S.text]];
				Raw.textarea ~a:[a_cols 80; a_rows 8; a_placeholder [%i18n S.quality_text]; a_name "quality_text"] (txt "");
				h3 [txt [%i18n S.evidence]];
				R.ol (quality_lis);
				ul [
					li [label [txt "Add URL "; txt " "; quality_url]];
					li [label [txt "Upload file"; txt " "; quality_file]]
				];
				h2 [txt [%i18n S.independence]];
				h3 [txt [%i18n S.text]];
				Raw.textarea ~a:[a_cols 80; a_rows 8; a_placeholder [%i18n S.independence_text]; a_name "independence_text"] (txt "");
				h3 [txt [%i18n S.evidence]];
				ul [
					li [Raw.input ~a:[a_input_type `File; a_class ["ot-pup-input"]; a_accept ["text/*"]] ()]
				];
				h2 [txt [%i18n S.communication]];
				h3 [txt [%i18n S.text]];
				Raw.textarea ~a:[a_cols 80; a_rows 8; a_placeholder [%i18n S.communication_text]; a_name "communication_text"] (txt "");
				h3 [txt [%i18n S.evidence]];
				ul [
					li [Raw.input ~a:[a_input_type `File; a_class ["ot-pup-input"]; a_accept ["text/*"]] ()]
				];
				table [
					tr [
						td [button ~a:[a_button_type `Button; a_class ["button"]] [txt [%i18n S.save]]];
						td [button ~a:[a_button_type `Submit; a_class ["button"]] [txt [%i18n S.submit]]]
					]
				]
			]) ()
		]
	]

let%shared do_report_feedback myid () (sid, (qf, (qg, (inf, (ing, (cf, cg)))))) =
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt () = set_report_feedback (ayear, sid, qf, qg, inf, ing, cf, cg) in
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared report_feedback_handler myid () () =
	try%lwt
		let%lwt t = Moab_users.get_user_type myid in
		match t with
		| Admin ->
			let%lwt form = F.Form.lwt_post_form ~service:report_feedback_action (fun (pid, (qf, (qg, (inf, (ing, (cf, cg)))))) ->
				let%lwt sw = Moab_students.student_select_widget (`Param pid) in
				Lwt.return @@ [table ~a:[a_class ["report-feedback"]] [
					tr [td ~a:[a_colspan 2] [sw]];
					tr [th []; th [txt [%i18n S.quality]]];
					tr [th [txt [%i18n S.comments]]; td [F.Form.textarea ~a:[a_cols 80; a_rows 8] ~name:qf ()]];
					tr [th [txt [%i18n S.grade]]; td [F.Form.input ~input_type:`Number ~name:qg F.Form.int]];
					tr [th []; th [txt [%i18n S.independence]]];
					tr [th [txt [%i18n S.comments]]; td [F.Form.textarea ~a:[a_cols 80; a_rows 8] ~name:inf ()]];
					tr [th [txt [%i18n S.grade]]; td [F.Form.input ~input_type:`Number ~name:ing F.Form.int]];
					tr [th []; th [txt [%i18n S.communication]]];
					tr [th [txt [%i18n S.comments]]; td [F.Form.textarea ~a:[a_cols 80; a_rows 8] ~name:cf ()]];
					tr [th [txt [%i18n S.grade]]; td [F.Form.input ~input_type:`Number ~name:cg F.Form.int]];
					tr [td ~a:[a_colspan 2] [
 						F.Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:[%i18n S.submit] F.Form.string
					]]
				]]) () in
			Moab_container.page (Some myid) [
				div ~a:[a_class ["content-box"]] [
					h1 [txt [%i18n S.report_feedback]];
					form
				]
			]
		| _ -> Moab_container.page (Some myid) []
	with
	| Failure x -> Moab_container.page (Some myid) [p [txt x]]
	| e -> Lwt.fail e

let%shared () =
	Eliom_registration.Any.register ~service:report_feedback_action
		(Os_session.connected_fun do_report_feedback);
