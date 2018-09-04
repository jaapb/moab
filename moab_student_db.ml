open Os_db
open Lwt

let set_student_info userid mdx_id =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "INSERT INTO moab.students \
			(userid, student_id) \
			VALUES \
			($userid, $mdx_id) \
			ON CONFLICT (userid) DO UPDATE SET student_id = EXCLUDED.student_id")

