[%%shared
	open Eliom_parameter
	open Eliom_service
]

let attendance_service = create ~path:(Path ["attendance"]) ~meth:(Get unit) ();;
let login_service = create ~path:No_path
  ~meth:(Post (unit, (string "name" ** string "password"))) ();;
let logout_service = create ~path:No_path
  ~meth:(Post (unit, unit)) ();;
let main_service = create ~path:(Path []) ~meth:(Get unit) ();;
