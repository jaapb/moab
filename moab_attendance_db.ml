open Os_db
open Lwt

let get_attendance session_id learning_week =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT u.userid, student_id, firstname, lastname \
			FROM ocsigen_start.users u JOIN moab.students st \
				ON u.userid = st.userid JOIN \
			moab.attendance a ON u.userid = a.userid \
			WHERE a.session_id = $session_id \
				AND learning_week = $learning_week")

let add_attendance session_id userid learning_week =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"INSERT INTO moab.attendance \
			(session_id, userid, learning_week) \
			VALUES \
			($session_id, $userid, $learning_week) \
			ON CONFLICT DO NOTHING")

let get_week_attendance userid ayear year learning_week =
	Moab_session_db.get_week_sessions ayear year learning_week >>=
	fun total_sids -> full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT s.session_id \
			FROM moab.sessions s JOIN moab.terms t
				ON s.academic_year = t.academic_year AND s.term_id = t.term_id \
				JOIN moab.attendance a \
				ON s.session_id = a.session_id \
			WHERE s.academic_year = $ayear \
				AND year = $year \
				AND learning_week = $learning_week \
				AND userid = $userid") >>=
	fun attended_sids ->
		Lwt.return (List.length attended_sids, List.length total_sids)
