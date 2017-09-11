open Lwt

module Lwt_thread = struct
        include Lwt
        let close_in = Lwt_io.close
        let really_input = Lwt_io.read_into_exactly
        let input_binary_int = Lwt_io.BE.read_int
        let input_char = Lwt_io.read_char
        let output_string = Lwt_io.write
        let output_binary_int = Lwt_io.BE.write_int
        let output_char = Lwt_io.write_char
        let flush = Lwt_io.flush
        let open_connection x = Lwt_io.open_connection x
        type out_channel = Lwt_io.output_channel
        type in_channel = Lwt_io.input_channel
end
module PGOCaml = PGOCaml_generic.Make(Lwt_thread)

let db_handler = ref None;;

let database_server = ref "";;
let database_port = ref None;;
let database_name = ref "";;
let database_user = ref "";;
let database_password = ref None;;

let get_db () =
	match !db_handler with
	| Some h -> return h
	| None -> begin
			PGOCaml.connect ~host:!database_server ?port:!database_port ~database:!database_name ~user:!database_user ?password:!database_password () >>=
			fun dbh -> db_handler := Some dbh; return dbh
		end
;;

let find_user user_id =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT is_admin \
		FROM users \
		WHERE user_id = $user_id" >>=
	function
	| [] -> Lwt.fail Not_found
	| [f] -> Lwt.return f
	| _ -> Lwt.fail_with "multiple users found"
;;

let find_sessions_now () =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT type \
		FROM sessions \
		WHERE year = EXTRACT(year FROM now())
		AND EXTRACT(week FROM now()) BETWEEN start_week AND end_week
		AND weekday = EXTRACT(dow FROM now())
		AND localtime BETWEEN start_time AND end_time" >>=
	function
	| [] -> Lwt.fail Not_found
	| ["L"] -> Lwt.return `Lecture
	| ["S"] -> Lwt.return `Seminar
	| ["T"] -> Lwt.return `Test
	| _ -> Lwt.fail_with "multiple sessions found"
;;
