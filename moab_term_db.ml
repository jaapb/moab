open Os_db
open Lwt

let get_terms () =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT DISTINCT(term) \
			FROM moab.term_sessions")

let add_term term year start_week end_week =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "INSERT INTO moab.term_sessions \
			(term, year, start_week, end_week) \
			VALUES
			($term, $year, $start_week, $end_week)")
