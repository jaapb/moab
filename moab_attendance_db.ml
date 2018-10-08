open Os_db
open Lwt

let get_session_attendance session_id learning_week =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT u.userid, student_id, firstname, lastname \
			FROM ocsigen_start.users u JOIN moab.students st \
				ON u.userid = st.userid JOIN \
			moab.attendance a ON u.userid = a.userid \
			WHERE a.session_id = $session_id \
				AND learning_week = $learning_week")

let add_session_attendance session_id userid learning_week =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"INSERT INTO moab.attendance \
			(session_id, userid, learning_week) \
			VALUES \
			($session_id, $userid, $learning_week) \
			ON CONFLICT DO NOTHING")

let get_week_attendance userid ayear learning_week =
	Moab_student_db.get_group_number ayear userid >>=
	function
	| None -> Lwt.fail_with "user does not have group number"
	| Some group_number -> begin
		Moab_session_db.get_week_sessions ayear learning_week group_number >>=
		fun total_sids -> full_transaction_block (fun dbh -> PGSQL(dbh)
			"SELECT s.session_id \
				FROM moab.sessions s JOIN moab.terms t
					ON s.academic_year = t.academic_year AND s.term_id = t.term_id \
					JOIN moab.attendance a \
					ON s.session_id = a.session_id \
				WHERE s.academic_year = $ayear \
					AND learning_week = $learning_week \
					AND userid = $userid") >>=
		fun attended_sids ->
			Lwt.return (List.length attended_sids, List.length total_sids)
		end	

let get_attendance_list ayear learning_week =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT st.userid, COUNT(s.session_id), COUNT(a.learning_week) \
			FROM moab.students st JOIN moab.sessions s ON st.group_number = s.group_number OR s.group_number IS NULL \
			CROSS JOIN generate_series(1,24) gs(week) \
			LEFT JOIN moab.attendance a ON a.userid = st.userid \
				AND a.session_id = s.session_id \
				AND a.learning_week = gs.week
			WHERE st.academic_year = $ayear AND gs.week <= $learning_week \
			GROUP BY st.userid \
			ORDER BY 3 ASC, 2 DESC") >>=
	Lwt_list.map_s (function
	| (uid, Some c1, Some c2) -> Lwt.return (uid, c1, c2)
	| _ -> Lwt.fail_with "get_attendance_list: COUNT returned null value")
