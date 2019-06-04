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

let get_scores ayear scorer_id presenter_id =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT criterion_id, score, comment \
			FROM moab.presentation_scores \
			WHERE academic_year = $ayear \
			AND scorer_id = $scorer_id \
			AND presenter_id = $presenter_id"
	)

let set_admin_scores ayear presenter_id topic duration pgrade fgrade comments =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "INSERT INTO moab.presentation_admin_scores \
			(academic_year, presenter_id, topic, duration, provisional_grade, final_grade, comments) \
			VALUES \
			($ayear, $presenter_id, $topic, $duration, $pgrade, $?fgrade, $comments) \
			ON CONFLICT (academic_year, presenter_id) DO UPDATE \
				SET topic = EXCLUDED.topic, duration = EXCLUDED.duration, \
				provisional_grade = EXCLUDED.provisional_grade, comments = EXCLUDED.comments"
	)

let get_admin_scores ayear presenter_id =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT topic, duration, provisional_grade, final_grade, comments \
			FROM moab.presentation_admin_scores \
			WHERE academic_year = $ayear \
				AND presenter_id = $presenter_id"
	) >>=	
	function
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail (Invalid_argument "get_admin_scores: found multiple results")

let get_average_scores ayear presenter_id =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT criterion_id, round(AVG(score), 1)::float \
			FROM moab.presentation_scores \
			WHERE academic_year = $ayear \
			AND presenter_id = $presenter_id \
			GROUP BY criterion_id, presenter_id \
			ORDER BY criterion_id") >>=
	function
	| [] -> Lwt.fail Not_found
	| l -> Lwt_list.map_s (fun (id, avg) ->
					match avg with
					| None -> Lwt.return (id, 0.0)
					| Some a -> Lwt.return (id, a)
				) l

let get_comments ayear presenter_id =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT criterion_id, comment \
			FROM moab.presentation_scores \
			WHERE academic_year = $ayear \
			AND presenter_id = $presenter_id \
			AND comment IS NOT NULL \
			ORDER BY criterion_id") >>=
	Lwt_list.map_s (fun (id, c) ->
		match c with
		| None -> Lwt.fail (Invalid_argument "get_comments: null comment found")
		| Some x -> Lwt.return (id, x)
	)

let get_possible_presentations ayear user_id learning_week =
	full_transaction_block (fun dbh ->
		match learning_week with
		| None -> PGSQL(dbh) "SELECT COUNT(DISTINCT sc.presenter_id) \
			FROM moab.presentation_scores sc \
			JOIN moab.presentation_schedule ps ON sc.presenter_id = ps.userid \
			JOIN moab.students s ON ps.group_number = s.group_number AND ps.academic_year = s.academic_year \
			WHERE ps.academic_year = $ayear \
			AND s.userid = $user_id"
		| Some lw -> PGSQL(dbh) "SELECT COUNT(DISTINCT sc.presenter_id) \
			FROM moab.presentation_scores sc \
			JOIN moab.presentation_schedule ps ON sc.presenter_id = ps.userid \
			JOIN moab.students s ON ps.group_number = s.group_number AND ps.academic_year = s.academic_year \
			WHERE ps.academic_year = $ayear \
			AND s.userid = $user_id \
			AND learning_week <= $lw") >>=
	function
	| [] -> Lwt.fail (Invalid_argument "get_possible_presentations: COUNT returned no rows")
	| [Some x] -> Lwt.return x
	| [None] -> Lwt.fail (Invalid_argument "get_possible_presentations: COUNT returned NULL")
	| _ -> Lwt.fail (Invalid_argument "get_possible_presentations: COUNT returned multiple rows")

let get_followed_presentations ayear user_id =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT COUNT(DISTINCT presenter_id) \
			FROM moab.presentation_scores \
			WHERE academic_year = $ayear \
				AND scorer_id = $user_id") >>=
	function
	| [] -> Lwt.fail (Invalid_argument "get_followed_presentations: COUNT returned no rows")
	| [Some x] -> Lwt.return x
	| [None] -> Lwt.fail (Invalid_argument "get_followed_presentations: COUNT returned NULL")
	| _ -> Lwt.fail (Invalid_argument "get_followed_presentations: COUNT returned multiple rows")
