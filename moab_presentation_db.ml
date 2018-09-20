open Os_db
open Lwt

let get_schedule ayear group_number =
	full_transaction_block (fun dbh -> PGSQL(dbh) "nullable-results"
		"SELECT gs.week, ps1.userid, ps2.userid \
		FROM generate_series(1,24) AS gs(week) \
			LEFT JOIN moab.presentation_schedule ps1 ON ps1.learning_week = gs.week \
				AND ps1.first_presenter \
				AND ps1.academic_year = $ayear AND ps1.group_number = $group_number \
			LEFT JOIN moab.presentation_schedule ps2 ON ps2.learning_week = gs.week \
				AND NOT ps2.first_presenter \
				AND ps2.academic_year = $ayear AND ps2.group_number = $group_number \
		ORDER BY gs.week ASC") >>=
	Lwt_list.map_s (function
	| (Some w, uid1, uid2) -> Lwt.return (w, uid1, uid2)
	| _ -> Lwt.fail (Invalid_argument "get_schedule found null week")
	)
