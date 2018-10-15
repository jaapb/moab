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

let get_week_attendance userid ayear term_id learning_week =
	Moab_student_db.get_group_number ayear userid >>=
	function
	| None -> Lwt.fail_with "user does not have group number"
	| Some group_number -> begin
		Moab_session_db.get_week_sessions ayear term_id group_number >>=
		fun total_sids -> full_transaction_block (fun dbh -> PGSQL(dbh)
			"SELECT s.session_id \
				FROM moab.sessions s 
					JOIN moab.attendance a \
					ON s.session_id = a.session_id \
				WHERE s.academic_year = $ayear \
					AND term_id = $term_id \
					AND learning_week = $learning_week \
					AND userid = $userid") >>=
		fun attended_sids ->
			Lwt.return (List.length attended_sids, List.length total_sids)
		end	

let get_attendance_list ayear learning_week =
	let ht = Hashtbl.create 1024 in
	Moab_term_db.get_learning_weeks ayear >>=
	Lwt_list.iteri_s (fun i (t, _, _) ->
		let week_nr = i + 1 in
		if week_nr <= learning_week then
		begin
			full_transaction_block (fun dbh -> PGSQL(dbh)
				"SELECT st.userid, COUNT(s.session_id) \
					FROM moab.students st JOIN moab.sessions s ON \
						st.group_number = s.group_number OR s.group_number IS NULL \
					WHERE $week_nr BETWEEN joined_week AND left_week OR (joined_week <= $week_nr AND left_week IS NULL) \
						AND s.term_id = $t \
					GROUP BY st.userid" >>=
			fun p -> PGSQL(dbh)
				"SELECT st.userid, COUNT(a.session_id) \
					FROM moab.students st LEFT JOIN moab.attendance a ON \
						st.userid = a.userid AND a.learning_week = $week_nr \
					GROUP BY st.userid" >>=
			fun a -> Lwt.return (p, a)) >>=
			fun (pos_l, att_l) -> Lwt_list.iter_s (function
			| (uid, Some p) ->
					(match Hashtbl.find_opt ht uid with
					| Some (pos, att) -> Hashtbl.replace ht uid (Int64.add pos p, att)
					| None -> Hashtbl.add ht uid (p, 0L));
					Lwt.return_unit
			| _ -> Lwt.fail_with "get_attendance_list: COUNT returned null value") pos_l >>=
			fun () -> Lwt_list.iter_s (function
			| (uid, Some a) ->
					(match Hashtbl.find_opt ht uid with
					| Some (pos, att) -> Hashtbl.replace ht uid (pos, Int64.add att a)
					| None -> Hashtbl.add ht uid (0L, a));
					Lwt.return_unit
			| _ -> Lwt.fail_with "get_attendance_list: COUNT returned null value") att_l
		end
		else
			Lwt.return_unit
	) >>=
	fun () -> Lwt.return (Hashtbl.fold (fun k (v1, v2) acc -> (k, v1, v2)::acc) ht [])
