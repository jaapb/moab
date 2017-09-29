open Lwt

exception No_group

let take n l =
	let rec take_aux n l res =
		if n <= 0 then List.rev res
		else
			match l with
			| [] -> List.rev res
			| h::t ->	take_aux (n-1) t (h::res) in
	take_aux n l []
;;

let rec drop n l =
	if n <= 0 then l
	else
		match l with
		| [] -> []
		| h::t -> drop (n-1) t
;;

module Lwt_thread = struct
        include Lwt
        let close_in = Lwt_io.close
        let really_input = Lwt_io.read_into_exactly
        let input_binary_int = Lwt_io.BE.read_int
        let input_char = Lwt_io.read_char
        let output_string = Lwt_io.write
        let output_binary_int = Lwt_io.BE.write_int
        let output_char = Lwt_io.write_char
        let flush = Lwt_io.flush
        let open_connection x = Lwt_io.open_connection x
        type out_channel = Lwt_io.output_channel
        type in_channel = Lwt_io.input_channel
end
module PGOCaml = PGOCaml_generic.Make(Lwt_thread)

let db_handler = ref None;;

let database_server = ref "";;
let database_port = ref None;;
let database_name = ref "";;
let database_user = ref "";;
let database_password = ref None;;

let get_db () =
	match !db_handler with
	| Some h -> return h
	| None -> begin
			PGOCaml.connect ~host:!database_server ?port:!database_port ~database:!database_name ~user:!database_user ?password:!database_password () >>=
			fun dbh -> db_handler := Some dbh; return dbh
		end
;;

let find_user user_id =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT id, name, is_admin \
		FROM users \
		WHERE id = upper($user_id)" >>=
	function
	| [] -> Lwt.fail Not_found
	| [f] -> Lwt.return f
	| _ -> Lwt.fail_with "multiple users found"
;;

let find_sessions_now () =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT s.id, type \
		FROM sessions s JOIN timetable t ON s.timetable_id = t.id \
		WHERE year = EXTRACT(year FROM now()) \
		AND EXTRACT(week FROM now()) BETWEEN start_week AND end_week \
		AND weekday = EXTRACT(dow FROM now()) \
		AND localtime BETWEEN start_time AND end_time" >>=
	function
	| [] -> Lwt.fail Not_found
	| [id, "L"] -> Lwt.return (id, `Lecture)
	| [id, "S"] -> Lwt.return (id, `Seminar)
	| [id, "T"] -> Lwt.return (id, `Test)
	| _ -> Lwt.fail_with "multiple sessions found"
;;

let has_attended session_id user_id week =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT session_id \
		FROM attendance \
		WHERE session_id = $session_id AND user_id = $user_id AND week = $week" >>=
	function
	| [] -> Lwt.return false
	| [s] -> Lwt.return true
	| _ -> Lwt.fail_with "multiple sessions found"
;;

let register_attendance session_id user_id week =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"INSERT INTO attendance (session_id, user_id, week) \
		VALUES \
		($session_id, $user_id, $week)"
;;

let log user_id ip_address thing =
	let str = match thing with
	| `No_session_found -> "S"
	| `External_address -> "X" in
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"INSERT INTO log (user_id, ip_address, time, action) \
		VALUES \
		($user_id, $ip_address, now (), $str)"
;;

let get_presentation_slots term group start_week =
	get_db () >>=
	fun dbh -> PGSQL(dbh) "nullable-results"
		"SELECT gs.week, sch1.user_id, u1.name, sch2.user_id, u2.name \
		FROM timetable t JOIN sessions s ON s.timetable_id = t.id \
		JOIN generate_series(1,53) AS gs(week) ON week BETWEEN start_week AND end_week \
		LEFT JOIN schedule sch1 ON sch1.week = gs.week AND sch1.first \
		LEFT JOIN schedule sch2 ON sch2.week = gs.week AND NOT sch2.first \
		LEFT JOIN users u1 ON sch1.user_id = u1.id \
		LEFT JOIN users u2 ON sch2.user_id = u2.id \
		WHERE t.group_number = $group AND term = $term AND type='S' \
		ORDER BY year ASC, week ASC" >>=
	fun l -> Lwt_list.map_s (function
	| (Some w, i1, n1, i2, n2) -> Lwt.return (Int32.to_int w, i1, n1, i2, n2)
	| _ -> Lwt.fail_with "NULL value in generated series (get_presentation_slots)"
	) (drop (start_week-1) l)
;;

let get_user_group user_id term =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT u.group_number, weekday \
		FROM users u JOIN timetable t ON u.group_number = t.group_number \
		WHERE u.id = $user_id AND t.term = $term" >>=
	function
	| [] -> Lwt.fail Not_found
	| [Some nr, wd] -> Lwt.return (nr, wd)
	| [None, wd] -> Lwt.fail No_group
	| _ -> Lwt.fail_with "multiple users found"
;;

let get_presenters term group week =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT user_id, first \
		FROM timetable t JOIN schedule sch ON sch.timetable_id = t.id \
		WHERE t.group_number = $group AND term = $term AND week = $week" >>=
	function
	| [] -> Lwt.return (None, None)
	| [u, true] -> Lwt.return (Some u, None)
	| [u, false] -> Lwt.return (None, Some u)
	| [u1, true; u2, false] -> Lwt.return (Some u1, Some u2)
	| [u1, false; u2, true] -> Lwt.return (Some u2, Some u1)
	| _ -> Lwt.fail_with "more than two presenters found"
;;

let get_learning_weeks group term =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT week, year \
		FROM timetable t JOIN sessions s ON t.id = s.timetable_id \
			JOIN generate_series(1,53) AS gs(week) ON gs.week BETWEEN start_week AND end_week \
			WHERE group_number = $group AND term = $term \
			ORDER by year ASC, week ASC" >>=
	fun l -> Lwt_list.map_s (function
	| None, _ -> Lwt.fail_with "NULL value in generated series (get_learning_weeks)"
	| Some w, y -> Lwt.return (Int32.to_int w, y)
	) l
;;

let get_blog uid week year =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT title, contents \
			FROM blogs
			WHERE user_id = $uid AND week = $week AND year = $year" >>=
	function
	| [] -> Lwt.fail Not_found
	| [t, c] -> Lwt.return (t, c)
	| _ -> Lwt.fail_with "multiple blogs found"
;;

let update_blog uid week year title text =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"INSERT INTO blogs (user_id, week, year, title, contents) VALUES
			($uid, $week, $year, $title, $text) \
			ON CONFLICT (user_id, week, year) DO UPDATE \
			SET title = EXCLUDED.title, contents = EXCLUDED.contents"
;;

let get_presentation_week uid term =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT week, first \
			FROM schedule sch JOIN timetable t ON sch.timetable_id = t.id \
			WHERE user_id = $uid AND term = $term" >>=
	function
	| [] -> Lwt.return None
	| [w, f] -> Lwt.return (Some (w, f))
	| _ -> Lwt.fail_with "multiple presentations found"
;;

let set_presenter user_id term group week first =
	get_db () >>=
	fun dbh -> PGOCaml.begin_work dbh >>=
	fun () -> PGSQL(dbh)
		"SELECT id FROM timetable \
			WHERE term = $term AND group_number = $group AND type = 'S'" >>=
	(function
	| [] -> PGOCaml.rollback dbh >>= fun () -> Lwt.fail_with "no timetabled session found"
	| [x] -> Lwt.return x
	| _ -> PGOCaml.rollback dbh >>= fun () -> Lwt.fail_with "multiple sessions found") >>=
	fun t_id -> PGSQL(dbh)
		"INSERT INTO schedule (timetable_id, user_id, week, first) \
		VALUES \
		($t_id, $user_id, $week, $first) ON CONFLICT (timetable_id, user_id) DO UPDATE \
			SET week = $week, first = $first" >>=
	fun () -> PGOCaml.commit dbh
;;
