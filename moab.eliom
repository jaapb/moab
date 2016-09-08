[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
		open Eliom_service
		open Eliom_parameter
]

module Moab_app =
  Eliom_registration.App (
    struct
      let application_name = "moab"
      let global_data_path = None
    end)

let main_service = create ~id:(Path [])
	~meth:(Get unit)
	();;
let login_service = create ~id:Global
  ~meth:(Post (unit, (string "name" ** string "password")))
	();;
let logout_service = create ~id:Global
  ~meth:(Post (unit, unit))
 ();;

let user = Eliom_reference.eref ~scope:Eliom_common.default_session_scope
  None;;
let login_err = Eliom_reference.eref ~scope:Eliom_common.request_scope
  None;;

let login_action () (name, password) =
  let%lwt u = Lwt.return (Some ("Jaap1", "Jaap Boender", true)) in
  match u with
  | Some (uid, name, is_admin) -> Eliom_reference.set user (Some (uid, name, is_admin))
  | None -> Eliom_reference.set login_err (Some "Unknown user or wrong password")
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
      (*tr [
        td ~a:[a_colspan 3]
          [a ~service:register_service [pcdata "Create a new account"] ()]
      ]::*)
      (match err with
      | None -> []
      | Some e -> [tr [td ~a:[a_colspan 3; a_class ["error"]] [pcdata e]]]
      )
    )]) ()]
  | Some (_, n, _) -> [Form.post_form ~service:logout_service (fun () ->
    [table [
      tr [
        td [pcdata (Printf.sprintf "Logged in as %s" n)]
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
      div ~a:[a_class ["layout"]; a_id "menu"] [];
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
				h1 [pcdata "Welcome"]
			]
	)
	(fun e -> error_page (Printexc.to_string e))
;;

let () =
  Moab_app.register ~service:main_service main_page;;
