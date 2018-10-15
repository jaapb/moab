open Os_db
open Lwt.Infix

let set_student_info userid ayear mdx_id joined_week =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "INSERT INTO moab.students \
			(userid, student_id, academic_year, joined_week) \
			VALUES 
			($userid, $mdx_id, $ayear, $joined_week) \
			ON CONFLICT (userid, academic_year) DO UPDATE \
				SET student_id = EXCLUDED.student_id, \
					joined_week = EXCLUDED.joined_week")

let get_group_number ayear userid =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT group_number \
			FROM moab.students \
			WHERE academic_year = $ayear AND userid = $userid") >>=
	function
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail (Invalid_argument "get_group_number found multiple users with same userid")

let set_group_number ayear userid group_number =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "UPDATE moab.students \
			SET group_number = $?group_number	\
			WHERE academic_year = $ayear AND userid = $userid")

let get_group_numbers ayear =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT DISTINCT(group_number) \
			FROM moab.students \
			WHERE group_number IS NOT NULL \
				AND academic_year = $ayear \
			ORDER BY 1 ASC") >>=
	Lwt_list.map_s @@ function
	| None -> Lwt.fail (Invalid_argument "get_group_numbers found an empty value after DISTINCT")
	| Some x -> Lwt.return x

let find_student student_id =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT userid \
			FROM moab.students \
			WHERE student_id = upper($student_id)") >>=
	function
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail (Invalid_argument "find_student found multiple students with same student_id")

let get_students ayear group_number =
	full_transaction_block (fun dbh -> 
		match group_number with
		| None -> PGSQL(dbh) "SELECT userid \
				FROM moab.students \
				WHERE academic_year = $ayear"
		| Some g -> PGSQL(dbh) "SELECT userid \
				FROM moab.students \
				WHERE academic_year = $ayear \
					AND group_number = $g")

let get_student_id uid = 
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT student_id \
			FROM moab.students \
			WHERE userid = $uid") >>=
	function
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail (Invalid_argument "get_name found multiple users with same uid")

let deactivate_student ayear uid week =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"UPDATE moab.students \
			SET left_week = $week \
			WHERE academic_year = $ayear AND userid = $uid")

let get_active_students ayear week group_number =
	full_transaction_block (fun dbh -> 
		match group_number with
		| None -> PGSQL(dbh) "SELECT userid \
				FROM moab.students \
				WHERE academic_year = $ayear \
					AND ($week BETWEEN joined_week AND left_week) OR ($week >= joined_week AND left_week IS NULL)"
		| Some g -> PGSQL(dbh) "SELECT userid \
				FROM moab.students \
				WHERE academic_year = $ayear \
					AND ($week BETWEEN joined_week AND left_week) OR ($week >= joined_week AND left_week IS NULL) \
					AND group_number = $g")
