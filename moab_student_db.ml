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

let get_students ayear =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT userid \
			FROM moab.students \
			WHERE academic_year = $ayear")
