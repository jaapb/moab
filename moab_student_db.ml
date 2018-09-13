open Os_db
open Lwt

let set_student_info userid ayear mdx_id joined_week =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "INSERT INTO moab.students \
			(userid, student_id, academic_year, joined_week) \
			VALUES 
			($userid, $mdx_id, $ayear, $joined_week) \
			ON CONFLICT (userid, academic_year) DO UPDATE \
				SET student_id = EXCLUDED.student_id, \
					joined_week = EXCLUDED.joined_week")

let get_group_number userid =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT group_number \
			FROM moab.students \
			WHERE userid = $userid") >>=
	function
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail (Invalid_argument "get_group_number found multiple users with same userid")

let set_group_number userid group_number =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "UPDATE moab.students \
			SET group_number = $?group_number	\
			WHERE userid = $userid")
