(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

let%shared os_header ?user () = Eliom_content.Html.F.(
  let%lwt user_box = Moab_users.user_box user in
	let%lwt tp = match user with
		| None -> Lwt.return Moab_users.Student
		| Some u -> Moab_users.get_user_type (Os_user.userid_of_user u) in
	let user_class = match tp with
	| Moab_users.Admin -> "header-admin"
	| Moab_users.Examiner -> "header-examiner"
	| Moab_users.Student -> "header-student" in
  Lwt.return (
    header ~a:[a_class ["os-page-header"; user_class]]
      [ a ~a:[a_class ["os-page-header-app-name"]]
          ~service:Os_services.main_service
          [ txt Moab_base.displayed_app_name ]
          ()
      ; user_box
      ]
  )
)

let%shared os_footer () = Eliom_content.Html.F.(
  footer ~a:[a_class ["os-page-footer"]] [
    p [
      txt [%i18n S.footer_generated];
      a ~service:Moab_services.os_github_service
        [ txt " Ocsigen Start " ] ();
      txt [%i18n S.footer_eliom_distillery];
      a ~service:Moab_services.ocsigen_service
        [ txt " Ocsigen " ] ();
      txt [%i18n S.footer_technology]
    ]
  ]

 )

let%server get_wrong_pdata () =
  Lwt.return @@ Eliom_reference.Volatile.get Os_msg.wrong_pdata

let%client get_wrong_pdata =
  ~%(Eliom_client.server_function [%derive.json : unit]
       (Os_session.connected_wrapper get_wrong_pdata))

let%shared connected_welcome_box () = Eliom_content.Html.F.(
  let%lwt wrong_pdata = get_wrong_pdata () in
  let info, ((fn, ln), (p1, p2)) =
    match wrong_pdata with
    | None ->
      p [
        txt [%i18n S.personal_information_not_set];
        br ();
        txt [%i18n S.take_time_enter_name_password];
      ], (("", ""), ("", ""))
    | Some wpd -> p [txt [%i18n S.wrong_data_fix]], wpd
  in
  Lwt.return @@
    div ~a:[a_class ["os-welcome-box"]] [
      div [h2 [%i18n welcome ~capitalize:true]; info];
      Os_user_view.information_form
        ~a_placeholder_password:[%i18n S.password]
        ~a_placeholder_retype_password:[%i18n S.retype_password]
        ~a_placeholder_firstname:[%i18n S.your_first_name]
        ~a_placeholder_lastname:[%i18n S.your_last_name]
        ~text_submit:[%i18n S.submit]
        ~firstname:fn ~lastname:ln
        ~password1:p1 ~password2:p2
        ()
    ]
)

let%shared get_user_data = function
  | None ->
    Lwt.return_none
  | Some myid ->
    let%lwt u = Os_user_proxy.get_data myid in
    Lwt.return_some (u)

let%shared page ?html_a ?a ?title ?head myid_o content =
  let%lwt me = get_user_data myid_o in
  let%lwt content = match me with
    | Some me when not (Os_user.is_complete me) ->
      let%lwt cwb = connected_welcome_box () in
      Lwt.return @@ cwb :: content
    | _ ->
      Lwt.return @@ content
  in
  let%lwt h = os_header ?user:me () in
	let%lwt drawer = Moab_drawer.make ?user:me () in
  Lwt.return
    (Os_page.content ?html_a ?a ?title ?head
       [ h
       ; Eliom_content.Html.F.(div ~a:[a_class ["os-body"]] content)
       ; os_footer ()
       ; drawer
       ])
