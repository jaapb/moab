(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

[%%shared
  open Eliom_content.Html.F
	open CalendarLib
]

(* Set personal data *)

let%server set_personal_data_handler =
  Os_session.connected_fun Os_handlers.set_personal_data_handler

let%client set_personal_data_handler =
  let set_personal_data_rpc =
    ~%(Eliom_client.server_function
         [%derive.json : ((string * string) * (string * string))]
         (Os_session.connected_wrapper (set_personal_data_handler ())))
  in
  fun () -> set_personal_data_rpc

(* Forgot password *)

let%server forgot_password_handler =
  Os_handlers.forgot_password_handler
    Moab_services.settings_service

let%client forgot_password_handler =
  let forgot_password_rpc =
    ~%(Eliom_client.server_function [%derive.json : string]
       (Os_session.connected_wrapper (forgot_password_handler ())))
  in
  fun () -> forgot_password_rpc

(* Action links are links created to perform an action. They are used
   for example to send activation links by email, or links to reset a
   password. You can create your own action links and define their
   behavior here. *)
let%shared action_link_handler myid_o akey () =
  (* We try first the default actions (activation link, reset
     password) *)
  try%lwt Os_handlers.action_link_handler myid_o akey () with
  | Os_handlers.No_such_resource
  | Os_handlers.Invalid_action_key _ ->
    Os_msg.msg ~level:`Err ~onload:true
      [%i18n S.invalid_action_key];
    Eliom_registration.(appl_self_redirect Action.send) ()
  | e ->
    let%lwt (email, phantom_user) =
      match e with
      | Os_handlers.Account_already_activated_unconnected
          {Os_types.Action_link_key.userid = _; email; validity = _;
           action = _; data = _; autoconnect = _} ->
        Lwt.return (email, false)
      | Os_handlers.Custom_action_link
          ({Os_types.Action_link_key.userid = _; email; validity = _;
            action = _; data = _; autoconnect = _},
           phantom_user) ->
        Lwt.return (email, phantom_user)
      | _ ->
        Lwt.fail e
    in
    (* Define here your custom action links. If phantom_user is true,
       it means the link has been created for an email that does not
       correspond to an existing user. By default, we just display a
       sign up form or phantom users, a login form for others.  You
       don't need to modify this if you are not using custom action
       links.

       Perhaps personalise the intended behavior for when you meet
       [Account_already_activated_unconnected].  *)
    (* if myid_o = None (* Not currently connected, and no autoconnect *)
    then
      if phantom_user
      then
        let page = [ div ~a:[ a_class ["login-signup-box"] ]
                       [ Os_user_view.sign_up_form
                           ~a_placeholder_email:[%i18n S.your_email]
                           ~text:[%i18n S.sign_up]
                           ~email
                           ()
                       ]
                   ]
        in
        Moab_base.App.send
          (Moab_page.make_page (Os_page.content page))
      else
        let page = [ div
                       ~a:[ a_class ["login-signup-box"] ]
                       [ Os_user_view.connect_form
                           ~a_placeholder_email:[%i18n S.your_email]
                           ~a_placeholder_pwd:[%i18n S.your_password]
                           ~text_keep_me_logged_in:[%i18n S.keep_logged_in]
                           ~text_sign_in:[%i18n S.sign_in]
                           ~email
                           ()
                       ]
                   ]
        in
        Moab_base.App.send
          (Moab_page.make_page (Os_page.content page))
    else (*VVV In that case we must do something more complex. Check
               whether myid = userid and ask the user what he wants to
               do. *) *)
      Eliom_registration.
        (appl_self_redirect
           Redirection.send
           (Redirection Eliom_service.reload_action))

(* Set password *)

let%server set_password_handler =
  Os_session.connected_fun
    (fun myid () (pwd, pwd2) ->
       let%lwt () = Os_handlers.set_password_handler myid () (pwd, pwd2) in
       Lwt.return
         (Eliom_registration.Redirection Eliom_service.reload_action))

let%client set_password_handler () (pwd, pwd2) =
  let%lwt () = Os_handlers.set_password_rpc (pwd, pwd2) in
  Lwt.return (Eliom_registration.Redirection Eliom_service.reload_action)

(* Preregister *)

let%server preregister_handler =
  Os_handlers.preregister_handler

let%client preregister_handler =
  let preregister_rpc =
    ~%(Eliom_client.server_function [%derive.json : string]
       (Os_session.connected_wrapper (preregister_handler ())))
  in
  fun () -> preregister_rpc

let%shared admin_dashboard () =
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt lw = Moab_terms.learning_week_of_date ayear (Date.today ()) in
	let%lwt sids = Moab_students.get_students ayear in
	Lwt.return [div ~a:[a_class ["content-box"]] [
		h1 [pcdata [%i18n S.dashboard]];
		p [
			pcdata [%i18n S.current_academic_year ~capitalize:true]; pcdata ": ";
			pcdata ayear; pcdata "; ";
			pcdata [%i18n S.learning_week]; pcdata ": ";
			(match lw with
			| None -> b [pcdata [%i18n S.none]]
			| Some l -> pcdata (string_of_int l)); pcdata "; ";
			pcdata [%i18n S.number_of_students]; pcdata ": ";
			pcdata (string_of_int (List.length sids))
		]
	]]

let%shared student_dashboard () =
	Lwt.return [div ~a:[a_class ["content-box"]] [
		h1 [pcdata [%i18n S.dashboard]]	
	]]

let%shared examiner_dashboard () =
	Lwt.return [div ~a:[a_class ["content-box"]] [
		h1 [pcdata [%i18n S.dashboard]]	
	]]

let%shared main_service_handler myid_o () () =
	let%lwt contents = match myid_o with
	| None -> Lwt.return [p [%i18n welcome_text1]]
	| Some myid -> Moab_users.(
		let%lwt tp = get_user_type myid in
		match tp with
		| Admin -> admin_dashboard ()
		| Student -> student_dashboard ()
		| Examiner -> examiner_dashboard ())
	in
  Moab_container.page
    ~a:[ a_class ["os-page-main"] ]
    myid_o
		contents

let%shared settings_handler myid_o () () =
  let%lwt content = match myid_o with
    | Some _ -> Moab_settings.settings_content ()
    | None -> Lwt.return [ p [%i18n log_in_to_see_page ~capitalize:true]]
  in
  Moab_container.page myid_o content

let%server update_language_handler () language =
  Os_session.connected_wrapper Moab_language.update_language
    (Moab_i18n.language_of_string language)

let%client update_language_handler () language =
  Moab_i18n.(set_language (language_of_string language));
  Os_current_user.update_language language

(* Connection *)

let connect_handler () ((login, pwd), keepmeloggedin) =
	Ocsigen_messages.console (fun () -> "[connect_handler]");
	try%lwt
		let%lwt userid = Moab_users.verify_password login pwd in
		let%lwt () = Os_handlers.disconnect_handler () () in
		Ocsigen_messages.console (fun () -> "- connecting...");
		Os_session.connect ~expire:(not keepmeloggedin) userid
	with
	| Os_db.No_such_resource ->
		Eliom_reference.Volatile.set Os_user.wrong_password true;
		Os_msg.msg ~level:`Err ~onload:true "Wrong password";
		Lwt.return_unit

let%server connect_handler_rpc v = connect_handler () v

let%client connect_handler_rpc =
	~%(Eliom_client.server_function
			~name:"Moab_handlers.connect_handler"
			[%derive.json: (string * string) * bool]
		connect_handler_rpc)

let%client connect_handler () v = connect_handler_rpc v
