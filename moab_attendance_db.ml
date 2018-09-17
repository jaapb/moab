open Os_db
open Lwt

let get_attendance session_id =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT u.userid, student_id, firstname, lastname \
			FROM ocsigen_start.users u JOIN moab.students st \
				ON u.userid = st.userid JOIN \
			moab.attendance a ON u.userid = a.userid \
			WHERE a.session_id = $session_id")
