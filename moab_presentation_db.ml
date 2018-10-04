open Os_db
open Lwt

let get_schedule ayear group_number =
	full_transaction_block (fun dbh -> PGSQL(dbh) "nullable-results"
		"SELECT gs.week, ps1.userid, ps2.userid \
		FROM generate_series(1,24) AS gs(week) \
			LEFT JOIN moab.presentation_schedule ps1 ON ps1.learning_week = gs.week \
				AND ps1.first_presenter \
				AND ps1.academic_year = $ayear \
			LEFT JOIN moab.students s1 ON ps1.userid = s1.userid \
			LEFT JOIN moab.presentation_schedule ps2 ON ps2.learning_week = gs.week \
				AND NOT ps2.first_presenter \
				AND ps2.academic_year = $ayear \
			LEFT JOIN moab.students s2 ON ps2.userid = s2.userid \
		WHERE (s1.group_number = $group_number OR s1.group_number IS NULL) \
		AND (s2.group_number = $group_number OR s2.group_number IS NULL) \
		ORDER BY gs.week ASC") >>=
	Lwt_list.map_s (function
	| (Some w, uid1, uid2) -> Lwt.return (w, uid1, uid2)
	| _ -> Lwt.fail (Invalid_argument "get_schedule found null week")
	)

let schedule_presentation ayear learning_week first userid =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"INSERT INTO moab.presentation_schedule \
			(academic_year, learning_week, first_presenter, userid) \
			VALUES \
			($ayear, $learning_week, $first, $userid) \
			ON CONFLICT (academic_year, userid) DO UPDATE \
				SET learning_week = EXCLUDED.learning_week, \
				first_presenter = EXCLUDED.first_presenter")
