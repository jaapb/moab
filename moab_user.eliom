[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
]

let verify_password userid password =
	Moab_user_db.verify_password userid password

let%shared connect_form () =
	D.Form.post_form ~service:Moab_services.connect_service
		(fun ((login, password), keepmeloggedin) ->
		[
			Form.input ~a:[a_placeholder [%i18n S.your_login]]
				~name:login ~input_type:`Text Form.string;
			Form.input ~a:[a_placeholder [%i18n S.your_password]]
				~name:password ~input_type:`Password Form.string;
			label [
				Form.bool_checkbox_one ~a:[a_checked ()] ~name:keepmeloggedin ();
				pcdata [%i18n S.keep_logged_in]
			];
			Form.input ~a:[a_class ["button"; "os-sign-in"]]
				~input_type:`Submit ~value:[%i18n S.sign_in] Form.string
		]) ()

let%shared forgot_password_form () =
	Os_user_view.generic_email_form ~service:Os_services.forgot_password_service ()

let%shared bind_popup_button ?a ~button
	~(popup_content: ((unit -> unit Lwt.t) -> [< Html_types.div_content]
		Eliom_content.Html.elt Lwt.t) Eliom_client_value.t) () =
	ignore [%client
		(Lwt.async (fun () ->
			Lwt_js_events.clicks (Eliom_content.Html.To_dom.of_element ~%button)
			(fun _ _ ->
				let%lwt _ = Ot_popup.popup ?a:~%a ~close_button:[Os_icons.F.close ()]
					~%popup_content in
				Lwt.return_unit))
		: _)
	]

let%shared forgotpwd_button close =
	let popup_content = [%client fun _ ->
		Lwt.return @@ div [
			h2 [pcdata [%i18n S.recover_password]];
			forgot_password_form ()
		]
	] in
	let button = D.Raw.a ~a:[a_class ["os-forgot-pwd-link"];
		a_onclick [%client fun _ -> ~%close ()]]
		[pcdata [%i18n S.forgot_your_password_q]] in
	bind_popup_button
		~a:[a_class ["os-forgot-pwd"]]
		~button
		~popup_content
		();
	button

let%shared sign_in_button () =
	let popup_content = [%client fun close -> Lwt.return @@
		div
		[ h2 [pcdata [%i18n S.sign_in]]
		; connect_form ()
		; forgotpwd_button (fun () -> Lwt.async close)
		]
	] in
	let button =
		D.button ~a:[a_class ["button"; "os-sign-in-btn"]]
			[pcdata [%i18n S.sign_in]] in
	bind_popup_button
		~a:[a_class ["os-sign-in"]]
		~button
		~popup_content
		();
	button

let%shared connected_user_box user =
	let username = Printf.sprintf "%s %s" (Os_user.firstname_of_user user)
		(Os_user.lastname_of_user user) in
	let avatar = match Os_user.avatar_uri_of_user user with
	| Some src -> img ~alt:"picture" ~a:[a_class ["os-avatar"]] ~src ()
	| None -> Os_icons.F.user () in
	D.div ~a:[a_class ["connected-user-box"]]
	[
		avatar;
		div [pcdata username]
	]

let%shared connection_box () =
	let sign_in = sign_in_button () in
	Lwt.return @@ div ~a:[a_class ["os-connection-box"]]
		[ sign_in ]

let%shared user_box user =
	match user with
	| None -> connection_box ()
	| Some user -> Lwt.return (connected_user_box user)