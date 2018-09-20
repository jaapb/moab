open Os_db
open Lwt

let get_blog userid ayear week =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT title, text \
			FROM moab.blogs \
			WHERE userid = $userid AND academic_year = $ayear AND learning_week = $week"
	) >>=
	function
	| [x] -> Lwt.return x
	| [] -> Lwt.fail Not_found
	| _ -> Lwt.fail (Invalid_argument "get_blog found multiple blogs")

let update_blog userid ayear week title text =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"INSERT INTO moab.blogs \
			(userid, academic_year, learning_week, title, text) \
			VALUES \
			($userid, $ayear, $week, $title, $text) \
			ON CONFLICT (userid, academic_year, learning_week) DO UPDATE \
				SET title = EXCLUDED.title, text = EXCLUDED.text")

let get_nr_blogs userid ayear =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT COUNT(title) \
			FROM moab.blogs \
			WHERE userid = $userid AND academic_year = $ayear") >>=
	function
	| [Some x] -> Lwt.return x
	| [] -> Lwt.fail Not_found
	| _ -> Lwt.fail (Invalid_argument "get_nr_blogs returned a strange result")
