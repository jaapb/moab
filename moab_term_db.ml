open Os_db
open Lwt

let get_academic_years () =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT DISTINCT(academic_year) \
			FROM moab.terms \
			ORDER BY 1 DESC")

let add_term ayear year start_week end_week =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "INSERT INTO moab.terms \
			(academic_year, year, start_week, end_week) \
			VALUES
			($ayear, $year, $start_week, $end_week)")

let get_learning_weeks ayear =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT term_id, week, year \
			FROM moab.terms t \
			JOIN generate_series(1,53) gs(week) ON gs.week BETWEEN start_week AND end_week \
			WHERE academic_year = $ayear \
			ORDER BY year ASC, week ASC") >>=
	Lwt_list.map_s (function
	| _, None, _ -> Lwt.fail (Invalid_argument "get_learning_weeks has a NULL value in generated series")
	| t, Some w, y -> Lwt.return (t, w, y))

let get_term_ids ayear =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT term_id \
			FROM moab.terms \
			WHERE academic_year = $ayear \
			ORDER BY 1 ASC")
