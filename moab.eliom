[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
]

[%%server
	open Services
	open Ocsigen_extensions.Configuration
]

module Moab_app =
  Eliom_registration.App (
    struct
      let application_name = "moab"
      let global_data_path = None
    end)

let ldap_urls = ref []
let term = ref 0
let start_week = ref 0
let inside_res = ref []

let user = Eliom_reference.eref ~scope:Eliom_common.default_session_scope
  None;;
let login_err = Eliom_reference.eref ~scope:Eliom_common.request_scope
  None;;

let login_action () (name, password) =
	let do_login name =
	begin
		Lwt.catch (fun () ->
			let%lwt (user_id, fname, is_admin) = Moab_db.find_user name in
			Eliom_reference.set user (Some (user_id, fname, is_admin))
		)
		(function
		| Not_found -> Eliom_reference.set login_err (Some "Not registered for this module")
		| Failure s -> Eliom_reference.set login_err (Some (Printf.sprintf "Failure: %s" s))
		| e -> Eliom_reference.set login_err (Some (Printexc.to_string e))
		)
	end in
	match !ldap_urls with
	| [] -> do_login name
	| l ->
		if password = "" then
			Eliom_reference.set login_err (Some "Empty password")
		else
			try
			let conn = Ldap_funclient.init (List.rev l) in
				Ldap_funclient.bind_s ~who:(Printf.sprintf "Uni\\%s" name) ~cred:password ~auth_method:`SIMPLE conn;
				do_login name
			with
			| Ldap_types.LDAP_Failure (`INVALID_CREDENTIALS, _, _) ->
				Eliom_reference.set login_err (Some "Unknown user or wrong password")
			| Ldap_types.LDAP_Failure (_, s, _) ->
				Eliom_reference.set login_err (Some (Printf.sprintf "Failure: %s" s))
			| e ->
				Eliom_reference.set login_err (Some (Printexc.to_string e))
;;

let logout_action () () =
  Eliom_reference.set user None
;;

let login_box () =
  let%lwt u = Eliom_reference.get user in
  let%lwt err = Eliom_reference.get login_err in
  Lwt.return (match u with
  | None -> [Form.post_form ~service:login_service (fun (name, password) ->
    [table (
      tr [
        td [pcdata "Username"];
        td ~a:[a_colspan 2]
          [Form.input ~input_type:`Text ~name:name Form.string]
      ]::
      tr [
        td [pcdata "Password"];
        td [Form.input ~input_type:`Password ~name:password Form.string];
        td [Form.input ~input_type:`Submit ~value:"Login" Form.string]
      ]::
      (match err with
      | None -> []
      | Some e -> [tr [td ~a:[a_colspan 3; a_class ["error"]] [pcdata e]]]
      )
    )]) ()]
  | Some (n, fn, ad) -> [Form.post_form ~service:logout_service (fun () ->
    [table [
      tr [
        td [pcdata (Printf.sprintf "Logged in as %s%s" fn
					(if ad then " (admin)" else ""))]
      ];
      tr [
        td [Form.input ~input_type:`Submit ~value:"Logout" Form.string]
      ]
		]]) ()]
  )
;; 

let container cts_div =
	let%lwt box = login_box () in
	Lwt.return
	(Eliom_tools.F.html
		~title:"CSD 3600"
		~css:[["css"; "moab.css"]]
		Html.F.(body [
			div ~a:[a_class ["layout"]; a_id "header"] [h1 [pcdata "CSD 3600"]];
      div ~a:[a_class ["layout"]; a_id "logbox"] box;
      div ~a:[a_class ["layout"]; a_id "menu"] [
			];
      div ~a:[a_class ["layout"]; a_id "contents"] cts_div;
      div ~a:[a_class ["layout"]; a_id "footer"] [
        img ~alt:"Powered by Ocsigen"
        ~src:(make_uri ~service:(static_dir ())
          ["ocsigen-powered.png"]) ()
      ]
    ])
  )
;;

let error_page e =
	container
	[
		h1 [pcdata "Error"];
		p [pcdata e]
	]
;;

let main_page () () =
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get user in
		match u with
		| None -> container []
		| Some _ -> container
			[
				h1 [pcdata "Welcome"];
				ul [
					li [a ~service:attendance_service [pcdata "Attendance recording"] ()];
					li [pcdata "Presentation feedback"];
					li [a ~service:schedule_service [pcdata "Presentation schedule"] ()];
					li [pcdata "Blog"]
				]
			]
	)
	(fun e -> error_page (Printexc.to_string e))
;;

let ldap_configuration = element
	~name:"ldap"
	~pcdata:(fun s -> ldap_urls := s::!ldap_urls)
	()
;;

let database_server_el = element
	~name:"server" ~obligatory:true
	~pcdata:(fun s -> Moab_db.database_server := s) ();;
let database_port_el = element
	~name:"port"
	~pcdata:(fun s -> Moab_db.database_port := Some (int_of_string s)) ();;
let database_name_el = element
	~name:"name" ~obligatory:true
	~pcdata:(fun s -> Moab_db.database_name := s) ();;
let database_user_el = element
	~name:"user" ~obligatory:true
	~pcdata:(fun s -> Moab_db.database_user := s) ();;
let database_password_el = element
	~name:"password" ~pcdata:(fun s -> Moab_db.database_password := Some s) ();;
let database_el = element
	~name:"database" ~obligatory:true ~elements:[database_server_el;
		database_port_el; database_name_el; database_user_el; database_password_el]
	();;

let term_year = attribute
	~name:"year"
	~obligatory:true
	(fun s -> term := int_of_string s)
	;;
let term_pres_start = attribute
	~name:"pres_start"
	~obligatory:true
	(fun s -> start_week := int_of_string s)
	;;
let term_configuration = element
	~name:"term"
	~obligatory:true
	~attributes:[term_year; term_pres_start]
	();;

let network_inside_re = attribute
	~name:"re"
	~obligatory:true
	(fun s -> inside_res := (Re_str.regexp s)::!inside_res)
	;;
let network_inside = element
	~name:"inside"
	~obligatory:true
	~attributes:[network_inside_re]
	();;
let network_configuration = element
	~name:"network"
	~elements:[network_inside]
	();;

let () =
	Eliom_config.parse_config [ldap_configuration; database_el; term_configuration; network_configuration];
  Moab_app.register ~service:main_service main_page;
	Eliom_registration.Action.register ~service:login_service login_action;
	Eliom_registration.Action.register ~service:logout_service logout_action
;;
