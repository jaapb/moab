[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
]

(* Local services *)

let%server submit_report_action = Eliom_service.create_attached_post
	~name:"submit_report_action"
	~fallback:Moab_services.submit_report_service
	~post_params:(any)
	()

let%client submit_report_action = 
	~%submit_report_action

(* Database access *)

(* Utility functions *)

(* CLient-side functions *)

let%client submit_clicked r_d r_fv (* s *) ev =
	(* let draft = Eliom_content.Html.To_dom.of_input r_d in
	let fv = Eliom_content.Html.To_dom.of_input r_fv in
	let submit = Eliom_content.Html.To_dom.of_button s in 
	Lwt_js_events.clicks ~use_capture:true submit @@ fun ev _ -> *)
	Lwt.async (fun () ->
		let%lwt ok = Ot_popup.confirm [p [pcdata [%i18n S.final_version_warning1]];
				p [pcdata [%i18n S.final_version_warning2]];
				p [pcdata [%i18n S.final_version_warning3]]
		]
		 [pcdata [%i18n S.yes]] [pcdata [%i18n S.no]] in
		if not ok then
			Dom.preventDefault ev;
		Lwt.return_unit
	)

(* Handlers *)

let%shared do_submit_report myid () params =
	let params_tr = List.map (fun (n, v) ->
		tr [
			td [pcdata n];
			td [pcdata v]
		]) params in
	let page = [
		table (tr [th [pcdata "Name"]; th [pcdata "Value"]]::params_tr)
	] in	
	Moab_base.App.send
		(Moab_page.make_page (Os_page.content page))
	
let%shared submit_report_handler myid () () =
	Eliom_registration.Any.register ~service:submit_report_action
		(Os_session.connected_fun do_submit_report);
	let draft = Raw.input ~a:[a_input_type `Radio; a_name "is_draft"; a_value "true"; a_checked ()] () in
	let final_version = D.Raw.input ~a:[a_input_type `Radio; a_name "is_draft"; a_value "false"] () in
	let submit = D.button ~a:[a_button_type `Submit; a_class ["button"]; a_onclick [%client (submit_clicked ~%draft ~%final_version)]] 
		[pcdata [%i18n S.submit]] in
	(* ignore [%client (Lwt.async (submit_clicked ~%draft ~%final_version ~%submit): unit)]; *)
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]] [
			h1 [pcdata [%i18n S.submit_report]];
			Form.post_form ~service:submit_report_action (fun is_draft ->
			[
				table [
					tr [
						td [label [draft; pcdata " "; pcdata [%i18n S.draft]]];
						td [label [final_version; pcdata " "; pcdata [%i18n S.final_version]]]
					];
				];
				h2 [pcdata [%i18n S.introduction]];
				h3 [pcdata [%i18n S.text]];
				Raw.textarea ~a:[a_cols 70; a_rows 8; a_placeholder [%i18n S.introduction_text]; a_name "introduction_text"] (pcdata "");
				h2 [pcdata [%i18n S.quality]];
				h3 [pcdata [%i18n S.text]];
				Raw.textarea ~a:[a_cols 70; a_rows 8; a_placeholder [%i18n S.quality_text]; a_name "quality_text"] (pcdata "");
				h3 [pcdata [%i18n S.evidence]];
				Raw.input ~a:[a_input_type `File; a_class ["ot-pup-input"]; a_accept ["text/*"]] ();
				h2 [pcdata [%i18n S.independence]];
				h3 [pcdata [%i18n S.text]];
				Raw.textarea ~a:[a_cols 70; a_rows 8; a_placeholder [%i18n S.independence_text]; a_name "independence_text"] (pcdata "");
				h3 [pcdata [%i18n S.evidence]];
				Raw.input ~a:[a_input_type `File; a_class ["ot-pup-input"]; a_accept ["text/*"]] ();
				h2 [pcdata [%i18n S.communication]];
				h3 [pcdata [%i18n S.text]];
				Raw.textarea ~a:[a_cols 70; a_rows 8; a_placeholder [%i18n S.communication_text]; a_name "communication_text"] (pcdata "");
				h3 [pcdata [%i18n S.evidence]];
				Raw.input ~a:[a_input_type `File; a_class ["ot-pup-input"]; a_accept ["text/*"]] ();
				table [
					tr [
						td [button ~a:[a_button_type `Button; a_class ["button"]] [pcdata [%i18n S.save]]];
						td [submit]
					]
				]
			]) ()
		]
	]
