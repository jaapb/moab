[%%shared
	open Eliom_parameter
	open Eliom_service
]

let attendance_service = create ~path:(Path ["attendance"]) ~meth:(Get unit) ();;
let attendance_report_service = create ~path:(Path ["attendance_report"]) ~meth:(Get unit) ();;
let feedback_service = create ~path:(Path ["feedback"]) ~meth:(Get unit) ();;
let login_service = create ~path:(Path []) ~meth:(Get unit) ();;
let logout_service = create ~path:No_path
  ~meth:(Post (unit, unit)) ();;
let main_service = create ~path:(Path ["main"]) ~meth:(Get unit) ();;
let schedule_service = create ~path:(Path ["schedule"]) ~meth:(Get (opt (int "group_number"))) ();;
let user_data_service = create ~path:(Path ["user_data"]) ~meth:(Get unit) ();;
let view_blog_service = create ~path:(Path ["blog"]) ~meth:(Get (suffix (string "user_id" ** int "learning_week"))) ();;
let write_blog_service = create ~path:(Path ["write_blog"]) ~meth:(Get unit) ();;
