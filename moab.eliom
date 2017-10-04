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
	open CalendarLib
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

let logout_action () () =
  Eliom_reference.set user None
;;

let container cts_div =
	let%lwt x = Eliom_reference.get user in
	Eliom_registration.Html.send (Eliom_tools.F.html
		~title:"CSD 3600"
		~css:[["css"; "moab.css"]]
		Html.F.(body [
			div ~a:[a_class ["layout"]; a_id "header"] (
				h1 [a ~service:main_service [pcdata "CSD 3600"] ()]::
				(match x with
				| None -> []
				| Some (_, nm, is_admin) -> 
					[
						Form.post_form ~a:[a_id "logout_form"] ~service:logout_service
						(fun () -> [table [
							tr [td [pcdata (Printf.sprintf "Logged in as %s%s" nm (if is_admin then " (admin)" else ""))]];
							tr [td [Form.input ~input_type:`Submit ~value:"Logout" Form.string]]
						]]) ()
					]
				)
			);
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

let login_page () () =
	let do_login_action () (name, password) =
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
		| [] -> let%lwt e = Moab_db.check_password name password in
			(match e with
			| None -> do_login name
			| Some e -> Eliom_reference.set login_err (Some e))
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
	in
	let do_login_service = create_attached_post ~fallback:login_service
  	~post_params:(string "name" ** string "password") () in
	Eliom_registration.Action.register ~scope:Eliom_common.default_session_scope
		~service:do_login_service do_login_action;
  let%lwt u = Eliom_reference.get user in
  let%lwt err = Eliom_reference.get login_err in
	Eliom_reference.set login_err None >>=
	fun () -> match u with
	| None -> container
  	[Form.post_form ~service:do_login_service (fun (name, password) ->
   	 [table ~a:[a_id "login_table"] (
   	   tr [
   	     td [pcdata "Username"];
   	     td ~a:[a_colspan 2]
   	       [Form.input ~input_type:`Text ~name:name Form.string]
   	   ]::
   	   tr [
   	     td [pcdata "Password"];
   		   td [Form.input ~input_type:`Password ~name:password Form.string]
			 ]::
			 tr [ 
   	     td ~a:[a_colspan 2]
				 	[Form.input ~input_type:`Submit ~value:"Login" Form.string]
   	   ]::
   	   (match err with
   	   | None -> []
   	   | Some e -> [tr [td ~a:[a_colspan 3; a_class ["error"]] [pcdata e]]]
   	   )
   	 )]) ()]
	| Some u -> Eliom_registration.Redirection.send (Eliom_registration.Redirection main_service)
;; 

let main_page () () =
	let admin_page () =
		container
		[
			h1 [pcdata "Welcome"];
			p [pcdata "Admin page still under construction."];
		]
	in
	Lwt.catch (fun () ->
		let%lwt u = Eliom_reference.get user in
		match u with
		| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
		| Some (user_id, _, is_admin) ->
			if is_admin then
				admin_page ()
			else
				let%lwt pr = Moab_db.get_presentation_week user_id !term in
				let%lwt (group, weekday, locked) = Moab_db.get_user_group user_id !term in
				let%lwt weeks = Moab_db.get_learning_weeks group !term in
				let%lwt blogs = Moab_db.get_user_blogs user_id !term in
				let%lwt this_lw = Moab_db.current_learning_week group !term in
				let%lwt	last_lw = match this_lw with
				| None -> Moab_db.last_learning_week group !term
				| x -> Lwt.return x in
				let%lwt fb = match last_lw with
				| None -> Lwt.return []
				| Some x -> Moab_db.get_feedback_given user_id !term x in
				container
				[
					h1 [pcdata "Welcome"];
					ul [
						li [a ~service:attendance_service [pcdata "Attendance recording"] ()];
						li [a ~service:feedback_service [pcdata "Presentation feedback"] ()];
						li [a ~service:schedule_service [pcdata "Presentation schedule"] ()];
						li [a ~service:write_blog_service [pcdata "Write blog"] ()]
					];
					h2 [pcdata "Status"];
					ul [
						li [pcdata (Printf.sprintf "You are assigned to seminar group %d.\n" group)];
						li [match pr with
						| None ->
								(match locked with
								| Some true ->
									pcdata "You do not have a presentation time scheduled. Contact the module leader as soon as possible."
								| _ ->
									pcdata "You do not have a presentation time scheduled. You can schedule your session in the 'Presentation schedule' section or wait to be randomly assigned one.")
						| Some (p_lw, _ ) ->
							let (p_wk, p_yr) = List.nth weeks (p_lw-1) in	
							let (sd, _) = Date.week_first_last p_wk p_yr in
							let day = Date.add sd (Date.Period.day (weekday - 1)) in
								pcdata (Printer.Date.sprint "Your presentation is scheduled on %d %B %Y." day)
						];
						li [
							match last_lw with
							| None -> pcdata "I don't know what learning week it is, sorry."
							| Some lw -> pcdata (Printf.sprintf "You have written a blog for %d out of %d week(s) so far. %d have been approved." (List.length blogs) lw (List.length (List.filter (fun (_, a) -> a) blogs)))
						];
						li [
							let fbp = List.length fb in
							let fbg = List.length (List.filter (fun (_, x) -> x <> None) fb) in
							pcdata (Printf.sprintf "You have given feedback for %d out of %d presentations (%d%%) (this may be off by one or two if your session for this week has not yet taken place)." fbg fbp
								(if fbp = 0 then 0 else (fbg / fbp * 100)));
						]
					]
				])
	(function
	| Not_found -> error_page "You do not seem to have been assigned a group number. This should not happen."
	| e -> error_page (Printexc.to_string e))
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
  Eliom_registration.Any.register ~service:login_service login_page;
  Eliom_registration.Any.register ~service:main_service main_page;
	Eliom_registration.Action.register ~service:logout_service logout_action
;;
