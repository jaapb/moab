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

let find_sessions () =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT type, weekday, start_time, end_time \
		FROM sessions \
		WHERE year = EXTRACT(year FROM now ())"
;;


