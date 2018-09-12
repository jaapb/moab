open Os_db
open Lwt

let get_sessions ayear =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT term_id, session_id, session_type, weekday, start_time, end_time, room \
		FROM moab.sessions \
		WHERE academic_year = $ayear")
