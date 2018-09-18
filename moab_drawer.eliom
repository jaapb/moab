(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

[%%shared
   open Eliom_content.Html.F
]

(** This module defines the drawer menu *)

let%shared item text service =
  li [ a ~a:[ a_class ["os-drawer-item"] ] ~service [pcdata text] () ]

let%shared admin_menu () =
  Lwt.return @@
	[ item [%i18n S.settings ~capitalize:true] Moab_services.settings_service
	; item [%i18n S.setup_terms] Moab_services.setup_terms_service
	; item [%i18n S.setup_sessions ~capitalize:true] Moab_services.setup_sessions_service
	; item [%i18n S.add_students ~capitalize:true] Moab_services.add_students_service
	; item [%i18n S.register_attendance] Moab_services.register_attendance_service
  ; Eliom_content.Html.F.li
      [ Os_user_view.disconnect_link
          ~text_logout:[%i18n S.logout ~capitalize:true]
          ~a:[ a_class ["os-drawer-item"] ] ()
      ]
  ]

let%shared examiner_menu () =
  Lwt.return @@
	[ item [%i18n S.settings ~capitalize:true] Moab_services.settings_service
  ; Eliom_content.Html.F.li
      [ Os_user_view.disconnect_link
          ~text_logout:[%i18n S.logout ~capitalize:true]
          ~a:[ a_class ["os-drawer-item"] ] ()
      ]
  ]

let%shared student_menu () =
	Lwt.return @@
  [ item [%i18n S.settings ~capitalize:true] Moab_services.settings_service
	; item [%i18n S.write_blog ~capitalize:true] Moab_services.edit_blog_service
  ; Eliom_content.Html.F.li
      [ Os_user_view.disconnect_link
          ~text_logout:[%i18n S.logout ~capitalize:true]
          ~a:[ a_class ["os-drawer-item"] ] ()
      ]
  ]

let%shared make ?(user: Os_types.User.t option) () =
  let%lwt items =
		match user with
		| None -> Lwt.return []
		| Some u ->
			begin
				let%lwt tp = Moab_users.get_user_type (Os_user.userid_of_user u) in
				match tp with
				| Moab_users.Admin -> admin_menu ()
				| Moab_users.Examiner -> examiner_menu ()
				| Moab_users.Student -> student_menu ()
			end
  in
  let items =
    item [%i18n S.home ~capitalize:true] Os_services.main_service
    :: items
  in
  let menu = ul ~a:[a_class ["os-drawer-menu"]] items in
  let contents = match user with
    | None -> [ menu ]
    | Some user ->
      let user_box = Moab_users.connected_user_box user in
      [ user_box ; menu ]
  in
  let drawer, _, _ = Ot_drawer.drawer contents in
  Lwt.return drawer
