[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
]

(* Types *)

[%%shared
	type user_type = Admin | Examiner | Student
	[@@deriving json]
]

(* Database access *)

let%shared find_user e =
	Moab_user_db.find_user e

let%client find_user =
	~%(Eliom_client.server_function [%derive.json : string]
			(Os_session.connected_wrapper find_user))

let%server get_user_type u =
	let%lwt t = Moab_user_db.get_user_type u in
	if t = "A" then Lwt.return Admin
	else if t = "E" then Lwt.return Examiner
	else if t = "S" then Lwt.return Student
	else Lwt.fail (Invalid_argument "unknown user type in database")

let%client get_user_type =
	~%(Eliom_client.server_function [%derive.json : int64]
			(Os_session.connected_wrapper get_user_type))

let%server add_user (t, fn, ln, email, password) =
	let ut = match t with
	| Admin -> "A"
	| Examiner -> "E"
	| Student -> "S" in
	Moab_user_db.add_user ut fn ln email password

let%client add_user =
	~%(Eliom_client.server_function [%derive.json : user_type * string * string * string * string option]
			(Os_session.connected_wrapper add_user))

let%server verify_password email password =
	Moab_user_db.verify_password email password

let%server get_name uid =
	Moab_user_db.get_name uid

let%client get_name =
	~%(Eliom_client.server_function [%derive.json: int64]
			(Os_session.connected_wrapper get_name))

(* Handlers *)

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
				txt [%i18n S.keep_logged_in]
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
			h2 [txt [%i18n S.recover_password]];
			forgot_password_form ()
		]
	] in
	let button = D.Raw.a ~a:[a_class ["os-forgot-pwd-link"];
		a_onclick [%client fun _ -> ~%close ()]]
		[txt [%i18n S.forgot_your_password_q]] in
	bind_popup_button
		~a:[a_class ["os-forgot-pwd"]]
		~button
		~popup_content
		();
	button

let%shared sign_in_button () =
	let popup_content = [%client fun close -> Lwt.return @@
		div
		[ h2 [txt [%i18n S.sign_in]]
		; connect_form ()
		; forgotpwd_button (fun () -> Lwt.async close)
		]
	] in
	let button =
		D.button ~a:[a_class ["button"; "os-sign-in-btn"]]
			[txt [%i18n S.sign_in]] in
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
		div [txt username]
	]

let%shared connection_box () =
	let sign_in = sign_in_button () in
	Lwt.return @@ div ~a:[a_class ["os-connection-box"]]
		[ sign_in ]

let%shared user_box user =
	match user with
	| None -> connection_box ()
	| Some user -> Lwt.return (connected_user_box user)
