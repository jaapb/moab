open Os_db
open Lwt

let get_schedule ayear group_number =
	full_transaction_block (fun dbh -> PGSQL(dbh) "nullable-results"
		"SELECT gs.week, ps1.userid, ps2.userid \
		FROM generate_series(1,24) AS gs(week) \
			LEFT JOIN moab.presentation_schedule ps1 ON ps1.learning_week = gs.week \
				AND ps1.first_presenter \
				AND ps1.academic_year = $ayear \
				AND ps1.group_number = $group_number \
			LEFT JOIN moab.students s1 ON ps1.userid = s1.userid \
			LEFT JOIN moab.presentation_schedule ps2 ON ps2.learning_week = gs.week \
				AND NOT ps2.first_presenter \
				AND ps2.academic_year = $ayear \
				AND ps2.group_number = $group_number \
			LEFT JOIN moab.students s2 ON ps2.userid = s2.userid \
		ORDER BY gs.week ASC") >>=
	Lwt_list.map_s (function
	| (Some w, uid1, uid2) -> Lwt.return (w, (uid1, uid2))
	| _ -> Lwt.fail (Invalid_argument "get_schedule found null week")
	)

let schedule_presentation ayear learning_week gnr first userid =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"INSERT INTO moab.presentation_schedule \
			(academic_year, learning_week, group_number, first_presenter, userid) \
			VALUES \
			($ayear, $learning_week, $gnr, $first, $userid) \
			ON CONFLICT (userid, academic_year) DO UPDATE \
				SET learning_week = EXCLUDED.learning_week, \
				first_presenter = EXCLUDED.first_presenter,
				group_number = EXCLUDED.group_number")

let find_presentation ayear userid =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT learning_week, first_presenter \
			FROM moab.presentation_schedule \
			WHERE academic_year = $ayear \
				AND userid = $userid") >>=
	function
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail (Invalid_argument "find_presentation found multiple presentations for one userid")

let get_unassigned_students ayear gnr lw =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT s.userid \
			FROM moab.students s LEFT JOIN moab.presentation_schedule ps ON \
				s.userid = ps.userid AND s.academic_year = ps.academic_year \
			WHERE ($lw BETWEEN joined_week AND left_week OR (joined_week <= $lw AND left_week IS NULL)) \
			AND s.academic_year = $ayear AND s.group_number = $gnr \
			AND ps.learning_week IS NULL")

let get_random_unassigned_student ayear gnr lw =
	full_transaction_block (fun dbh ->
		get_unassigned_students ayear gnr lw >>=
		fun x -> let n = float_of_int (List.length x) in
			PGSQL(dbh) "SELECT s.userid \
			FROM moab.students s LEFT JOIN moab.presentation_schedule ps ON \
				s.userid  = ps.userid AND s.academic_year = ps.academic_year \
				WHERE ($lw BETWEEN joined_week AND left_week OR (joined_week <= $lw AND left_week IS NULL)) \
				AND s.academic_year = $ayear AND s.group_number = $gnr \
				AND ps.learning_week IS NULL \
				OFFSET floor(random() * $n) \
				LIMIT 1" >>=
			function
			| [] -> Lwt.return_none	
			| [uid] -> Lwt.return_some uid
			| _ -> Lwt.fail (Invalid_argument "get_random_unassigned_student: strange random result")
	)

let get_criteria ayear =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT id, criterion, description \
			FROM moab.presentation_criteria \
			WHERE academic_year = $ayear \
			ORDER BY id"
	)

let set_score ayear scorer_id presenter_id crit_id score comment =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "INSERT INTO moab.presentation_scores \
			(academic_year, scorer_id, presenter_id, criterion_id, score, comment) \
			VALUES \
			($ayear, $scorer_id, $presenter_id, $crit_id, $score, $comment) \
			ON CONFLICT (academic_year, scorer_id, presenter_id, criterion_id) DO UPDATE \
				SET score = EXCLUDED.score, comment = EXCLUDED.comment"
	)
