(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

[%%shared
	open Eliom_service
	open Eliom_parameter
]

let%server settings_service = create
  ~path:(Path ["settings"])
  ~meth:(Get unit)
  ()

let%server os_github_service =
  extern
    ~prefix:"http://github.com"
    ~path:["ocsigen"; "ocsigen-start"]
    ~meth:(Get unit)
    ()

let%server ocsigen_service =
  extern
    ~prefix:"http://ocsigen.org"
    ~path:[]
    ~meth:(Get unit)
    ()

let%server connect_service =
	create
		~name:"connect"
		~path:No_path
		~meth:(Post (unit, ((string "username" ** string "password") ** bool "keepmeloggedin")))
		()

let%server add_students_service =
	create
		~path:(Path ["add-students"])
		~meth:(Get unit)
		()

let%server setup_terms_service =
	create
		~path:(Path ["setup-terms"])
		~meth:(Get unit)
		()

let%server setup_sessions_service =
	create
		~path:(Path ["setup-sessions"])
		~meth:(Get unit)
		()

let%server edit_blog_service =
	create
		~path:(Path ["edit-blog"])
		~meth:(Get unit)
		()

let%client settings_service =
  ~%settings_service

let%client ocsigen_service =
  ~%ocsigen_service

let%client os_github_service =
  ~%os_github_service

let%client connect_service =
	~%connect_service

let%client add_students_service =
	~%add_students_service

let%client setup_terms_service =
	~%setup_terms_service

let%client setup_sessions_service =
	~%setup_sessions_service

let%client edit_blog_service =
	~%edit_blog_service

(* The OS lib needs access to the settings service to perform
   redirections to it. We need to register it *)
let%server () = Os_services.register_settings_service settings_service
