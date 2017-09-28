open Lwt

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

let get_presentation_slots term group =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT week, weekday \
		FROM timetable t JOIN sessions s ON s.timetable_id = t.id 
			JOIN generate_series(1,53) AS gs(week) ON \
			week BETWEEN start_week AND end_week \
		WHERE t.group_number = $?group AND term = $term AND type = 'S'" >>=
 	Lwt_list.map_s (function
		| (None, _) -> Lwt.fail_with "session found without a week"
		| (Some x, wd) -> Lwt.return (x, wd)) 
;;

let get_user_group user_id =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT group_number \
		FROM users \
		WHERE id = $user_id" >>=
	function
	| [] -> Lwt.fail Not_found
	| [nr] -> Lwt.return nr
	| _ -> Lwt.fail_with "multiple users found"
;;

let get_presenters group week weekday =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT u.name
		FROM timetable t JOIN sessions s ON t.id = s.timetable_id \
			JOIN schedule sch ON sch.session_id = s.id \ 
			JOIN users u ON sch.user_id = u.id \
		WHERE weekday = $weekday \
		AND sch.week = $week \
		AND sch.first \
		AND type = 'S' \
		AND t.group_number = $?group" >>=
	fun u1 -> PGSQL(dbh)
		"SELECT u.name
		FROM timetable t JOIN sessions s ON t.id = s.timetable_id \
			JOIN schedule sch ON sch.session_id = s.id \ 
			JOIN users u ON sch.user_id = u.id \
		WHERE weekday = $weekday \
		AND sch.week = $week \
		AND NOT sch.first \
		AND type = 'S' \
		AND t.group_number = $?group" >>=
	fun u2 -> match u1, u2 with
	| [], [] -> Lwt.return (None, None)
	| [p1], [] -> Lwt.return (Some p1, None)
	| [], [p2] -> Lwt.return (None, Some p2)
	| [p1], [p2] -> Lwt.return (Some p1, Some p2)
	| _ -> Lwt.fail_with "multiple sessions found"
;;

let get_learning_weeks group term =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT week, year \
		FROM timetable t JOIN sessions s ON t.id = s.timetable_id \
			JOIN generate_series(1,53) AS gs(week) ON gs.week BETWEEN start_week AND end_week \
			WHERE group_number = $?group AND term = $term \
			ORDER by year ASC, week ASC" >>=
	fun l -> Lwt_list.map_s (function
	| None, _ -> Lwt.fail_with "NULL value in generated series (get_learning_weeks)"
	| Some w, y -> Lwt.return (w, y)
	) l
;;

let get_blog uid week year =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"SELECT title, contents \
			FROM blogs
			WHERE uid = $uid AND week = $week AND year = $year" >>=
	function
	| [] -> Lwt.fail Not_found
	| [t, c] -> Lwt.return (t, c)
	| _ -> Lwt.fail_with "multiple blogs found"
;;

let update_blog uid week year title text =
	get_db () >>=
	fun dbh -> PGSQL(dbh)
		"INSERT INTO blogs (uid, week, year, title, contents) VALUES
			($uid, $week, $year, $title, $text) \
			ON CONFLICT (uid, week, year) DO UPDATE \
			SET title = EXCLUDED.title, contents = EXCLUDED.contents"
;;
