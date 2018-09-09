open Os_db
open Lwt

let get_blog userid term week =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT title, text \
			FROM moab.blogs \
			WHERE userid = $userid AND term = $term AND week = $week"
	) >>=
	function
	| [x] -> Lwt.return x
	| [] -> Lwt.fail Not_found
	| _ -> Lwt.fail (Invalid_argument "get_blog found multiple blogs")
