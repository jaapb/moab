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
				let%lwt tp = Moab_user.get_user_type (Os_user.userid_of_user u) in
				match tp with
				| Moab_user.Admin -> admin_menu ()
				| Moab_user.Examiner -> examiner_menu ()
				| Moab_user.Student -> student_menu ()
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
      let user_box = Moab_user.connected_user_box user in
      [ user_box ; menu ]
  in
  let drawer, _, _ = Ot_drawer.drawer contents in
  Lwt.return drawer
