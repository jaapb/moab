open Os_db
open Lwt

let get_terms () =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT DISTINCT(term) \
			FROM moab.term_sessions")
