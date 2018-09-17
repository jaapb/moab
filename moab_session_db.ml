open Os_db
open Lwt
open CalendarLib

let get_sessions ayear =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT term_id, session_id, session_type, weekday, start_time, end_time, room, group_number \
		FROM moab.sessions \
		WHERE academic_year = $ayear")

let get_fresh_session_id () =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT nextval('moab.sessions_session_id_seq'::regclass)") >>=
	function
	| [Some x] -> Lwt.return x
	| _ -> Lwt.fail (Invalid_argument "get_fresh_session_id returned something weird")

let add_session session_id ayear term_id session_type weekday start_time end_time room group_number =
	let st = Printer.Time.from_fstring "%H:%M:%S" (start_time ^ ":00") in
	let et = Printer.Time.from_fstring "%H:%M:%S" (end_time ^ ":00") in
	full_transaction_block (fun dbh -> match session_id with
	| None -> PGSQL(dbh) "INSERT INTO moab.sessions \
			(academic_year, term_id, session_type, weekday, start_time, end_time, room, group_number) \
			VALUES \
			($ayear, $term_id, $session_type, $weekday, $st, $et, $?room, $?group_number)"
	| Some sid -> PGSQL(dbh) "INSERT INTO moab.sessions \
			(session_id, academic_year, term_id, session_type, weekday, start_time, end_time, room, group_number) \
			VALUES \
			($sid, $ayear, $term_id, $session_type, $weekday, $st, $et, $?room, $?group_number) \
			ON CONFLICT (session_id, academic_year) DO UPDATE \
				SET term_id = EXCLUDED.term_id, session_type = EXCLUDED.session_type, \
					weekday = EXCLUDED.weekday, start_time = EXCLUDED.start_time, \
					end_time = EXCLUDED.end_time, room = EXCLUDED.room, \
					group_number = EXCLUDED.group_number")
 
