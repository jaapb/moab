open Os_db
open Lwt

let get_sessions ayear =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT term_id, session_id, session_type, weekday, start_time, end_time, room \
		FROM moab.sessions \
		WHERE academic_year = $ayear")

let get_fresh_session_id () =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT nextval('moab.sessions_session_id_seq'::regclass)") >>=
	function
	| [Some x] -> Lwt.return x
	| _ -> Lwt.fail (Invalid_argument "get_fresh_session_id returned something weird")
