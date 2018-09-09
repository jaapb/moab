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

let get_learning_weeks term =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT week, year \
			FROM moab.term_sessions t \
			JOIN generate_series(1,53) gs(week) ON gs.week BETWEEN start_week AND end_week \
			WHERE term = $term \
			ORDER BY year ASC, week ASC") >>=
	Lwt_list.map_s (function
	| None, _ -> Lwt.fail (Invalid_argument "get_learning_weeks has a NULL value in generated series")
	| Some w, y -> Lwt.return (w, y))
