open Os_db
open Lwt

let set_student_info userid term mdx_id joined_week left_week =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "INSERT INTO moab.students \
			(userid, student_id, term, joined_week, left_week) \
			VALUES \
			($userid, $mdx_id, $term, $joined_week, $?left_week) \
			ON CONFLICT (userid, term) DO UPDATE \
				SET student_id = EXCLUDED.student_id, \
					joined_week = EXCLUDED.joined_week, \
					left_week = EXCLUDED.left_week")

