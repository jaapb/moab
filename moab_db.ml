open Lwt
open CalendarLib

exception No_group

let rec find_nr f l s =
	match l with
	| [] -> raise Not_found
	| h::t -> if f h then s else find_nr f t (s+1)
;;

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

let database_server = ref "";;
let database_port = ref None;;
let database_name = ref "";;
let database_user = ref "";;
let database_password = ref None;;

let db_pool = Lwt_pool.create 5
	(fun () -> PGOCaml.connect ~host:!database_server ?port:!database_port
		~database:!database_name ~user:!database_user
		?password:!database_password ());;

let find_user user_id term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT id, first_name, last_name, is_admin \
		FROM users u LEFT JOIN students st ON u.id = st.user_id \
		WHERE id = upper($user_id) AND left_week IS NULL \
			AND (term = $term OR is_admin)") >>=
	function
	| [] -> Lwt.fail Not_found
	| [f] -> Lwt.return f
	| _ -> Lwt.fail_with "multiple users found"
;;

let find_sessions_now () =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT s.id, type, group_number \
		FROM sessions s JOIN timetable t ON s.timetable_id = t.id \
		WHERE year = EXTRACT(year FROM current_date) \
		AND EXTRACT(week FROM current_date) BETWEEN start_week AND end_week \
		AND weekday = EXTRACT(dow FROM current_date) \
		AND localtime BETWEEN start_time AND end_time") >>=
	function
	| [] -> Lwt.return (0l, `No_session, None)
	| [id, "L", g] -> Lwt.return (id, `Lecture, g)
	| [id, "S", g] -> Lwt.return (id, `Seminar, g)
	| [id, "T", g] -> Lwt.return (id, `Test, g)
	| _ -> Lwt.fail_with "multiple sessions found"
;;

let has_attended session_id user_id week =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT session_id \
		FROM attendance \
		WHERE session_id = $session_id AND user_id = $user_id \
		AND learning_week = $week") >>=
	function
	| [] -> Lwt.return false
	| [s] -> Lwt.return true
	| _ -> Lwt.fail_with "multiple sessions found"
;;

let register_attendance ?(need_confirmation=false) session_id user_id week =
	let confirmed = if need_confirmation then Some "W" else None in
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"INSERT INTO attendance (session_id, user_id, learning_week, confirmed) \
		VALUES \
		($session_id, $user_id, $week, $?confirmed)
		ON CONFLICT DO NOTHING")
;;

let log user_id ip_address thing =
	let str = match thing with
	| `No_session_found -> "S"
	| `External_address -> "X"
	| `Logged_in -> "L" in
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"INSERT INTO log (user_id, ip_address, time, action) \
		VALUES \
		($user_id, $ip_address, now(), $str)")
;;

let get_presentation_slots term group start_week =
	Lwt_pool.use db_pool (fun dbh -> PGOCaml.transact dbh (fun dbh ->
		PGSQL(dbh) "SELECT id \
			FROM timetable \
			WHERE term = $term AND group_number = $group AND type='S'" >>=
		(function
		| [] -> Lwt.fail Not_found
		| [id] -> Lwt.return id
		| _ -> Lwt.fail_with "multiple timetable slots found") >>=
		fun slot -> PGSQL(dbh) "nullable-results"
		"SELECT gs.week, sch1.user_id, u1.first_name, u1.last_name, \
		sch2.user_id, u2.first_name, u2.last_name \
		FROM generate_series(1,24) AS gs(week) \
		LEFT JOIN schedule sch1 ON sch1.learning_week = gs.week AND sch1.first \
			AND sch1.timetable_id = $slot \ 
		LEFT JOIN schedule sch2 ON sch2.learning_week = gs.week AND NOT sch2.first 
			AND sch2.timetable_id = $slot \
		LEFT JOIN users u1 ON sch1.user_id = u1.id \
		LEFT JOIN users u2 ON sch2.user_id = u2.id \
		ORDER BY gs.week ASC")) >>=
	fun l -> Lwt_list.map_s (function
	| (Some w, i1, fn1, ln1, i2, fn2, ln2) ->
		Lwt.return (Int32.to_int w, i1, fn1, ln1, i2, fn2, ln2)
	| _ -> Lwt.fail_with "NULL value in generated series (get_presentation_slots)"
	) (drop (start_week-1) l)
;;

let get_user_group user_id term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT st.group_number, weekday, locked \
		FROM users u JOIN students st ON u.id = st.user_id \
			JOIN timetable t ON st.group_number = t.group_number AND t.term = st.term \
		WHERE u.id = $user_id AND t.term = $term") >>=
	function
	| [] -> Lwt.fail Not_found
	| [nr, wd, l] -> Lwt.return (nr, wd, l)
	| _ -> Lwt.fail_with "multiple users found"
;;

let get_presenters term group week =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT user_id, u.first_name, u.last_name, first \
		FROM timetable t JOIN schedule sch ON sch.timetable_id = t.id \
		JOIN users u on u.id = sch.user_id \
		WHERE t.group_number = $group AND t.term = $term AND learning_week = $week") >>=
	function
	| [] -> Lwt.return (None, None)
	| [u, fn, ln, true] -> Lwt.return (Some (u, fn, ln), None)
	| [u, fn, ln, false] -> Lwt.return (None, Some (u, fn, ln))
	| [u1, fn1, ln1, true; u2, fn2, ln2, false] ->
		Lwt.return (Some (u1, fn1, ln1), Some (u2, fn2, ln2))
	| [u1, fn1, ln1, false; u2, fn2, ln2, true] ->
		Lwt.return (Some (u2, fn2, ln2), Some (u1, fn1, ln1))
	| _ -> Lwt.fail_with "more than two presenters found"
;;

let get_learning_weeks group term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT week, year \
		FROM timetable t JOIN sessions s ON t.id = s.timetable_id \
			JOIN generate_series(1,53) AS gs(week) ON gs.week BETWEEN start_week AND end_week \
			WHERE group_number = $group AND term = $term \
			ORDER by year ASC, week ASC") >>=
	Lwt_list.map_s (function
	| None, _ -> Lwt.fail_with "NULL value in generated series (get_learning_weeks)"
	| Some w, y -> Lwt.return (Int32.to_int w, y)
	)
;;

let get_blog uid week term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT title, contents \
			FROM blogs
			WHERE user_id = $uid AND learning_week = $week AND term = $term") >>=
	function
	| [] -> Lwt.fail Not_found
	| [t, c] -> Lwt.return (t, c)
	| _ -> Lwt.fail_with "multiple blogs found"
;;

let update_blog uid week term title text =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"INSERT INTO blogs (user_id, learning_week, term, title, contents) VALUES
			($uid, $week, $term, $title, $text) \
			ON CONFLICT (user_id, learning_week, term) DO UPDATE \
			SET title = EXCLUDED.title, contents = EXCLUDED.contents")
;;

let get_presentation_week uid term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT learning_week, first \
			FROM schedule sch JOIN timetable t ON sch.timetable_id = t.id \
			WHERE user_id = $uid AND term = $term") >>=
	function
	| [] -> Lwt.return None
	| [w, f] -> Lwt.return (Some (w, f))
	| _ -> Lwt.fail_with "multiple presentations found"
;;

let set_presenter user_id term group week first =
	Lwt_pool.use db_pool (fun dbh -> PGOCaml.begin_work dbh >>=
	fun () -> PGSQL(dbh)
		"SELECT id FROM timetable \
			WHERE term = $term AND group_number = $group AND type = 'S'" >>=
	(function
	| [] -> PGOCaml.rollback dbh >>= fun () -> Lwt.fail_with "no timetabled session found"
	| [x] -> Lwt.return x
	| _ -> PGOCaml.rollback dbh >>= fun () -> Lwt.fail_with "multiple sessions found") >>=
	fun t_id -> PGSQL(dbh)
		"INSERT INTO schedule (timetable_id, user_id, learning_week, first) \
		VALUES \
		($t_id, $user_id, $week, $first) \
		ON CONFLICT (timetable_id, user_id) DO UPDATE \
			SET learning_week = $week, first = $first" >>=
	fun () -> PGOCaml.commit dbh)
;;

let get_user_blogs user_id term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
	"SELECT learning_week, approved \
		FROM blogs \
		WHERE user_id = $user_id AND term = $term \
		ORDER BY learning_week ASC")
;;		

let current_learning_week group term =
	let now = Date.today () in
	get_learning_weeks group term >>=
	fun lws -> Lwt.catch (fun () -> Lwt.return
		(Some (find_nr (fun (w, y) -> w = (Date.week now) && y = (Date.year now)) lws 1))
	)
	(function
	| Not_found -> Lwt.return None
	| e -> Lwt.fail e)
;;

let last_learning_week group term =
	let now = Date.today () in
	get_learning_weeks group term >>=
	fun lws -> Lwt.catch (fun () -> Lwt.return
		(Some (find_nr (fun (w, y) -> w = (Date.week now) && y = (Date.year now)) lws 1))
	)
	(function
	| Not_found -> Lwt.catch (fun () -> Lwt.return
		(Some (find_nr (fun (w, y) -> w >= (Date.week now) && y = (Date.year now)) lws 1))
		)
		(function
		| Not_found -> Lwt.catch (fun () -> Lwt.return
			(Some (find_nr (fun (w, y) -> y = (Date.year now) + 1) lws 1))
			)
			(function
			| Not_found -> Lwt.return None
			| e -> Lwt.fail e)
		| e -> Lwt.fail e)
	| e -> Lwt.fail e)

let get_feedback_given user_id term learning_week =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh) 
	"SELECT DISTINCT(presenter_id), learning_week \
		FROM presentation_scores ps JOIN schedule sch ON ps.presenter_id = sch.user_id \
		JOIN students st ON ps.scorer_id = st.user_id \
		WHERE ps.term = $term AND scorer_id = $user_id \
		AND (learning_week BETWEEN st.joined_week AND st.left_week OR \
				(learning_week >= st.joined_week AND st.left_week IS NULL)) \
		AND	learning_week <= $learning_week")
;;

let get_feedback_possible user_id term learning_week =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh) 
		"SELECT DISTINCT(presenter_id), learning_week \
			FROM presentation_scores ps JOIN schedule sch ON ps.presenter_id = sch.user_id \
			JOIN timetable t ON sch.timetable_id = t.id \
			WHERE group_number = (SELECT group_number FROM students WHERE user_id = $user_id) \
			AND presenter_id <> $user_id \
			AND ps.term = $term AND learning_week <= $learning_week")
;;

let check_password user_id password =
	if password = "" then Lwt.return (Some "Empty password")
	else
		Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT password = crypt($password, password) \
			FROM users \
			WHERE id = upper($user_id)") >>=
		function
		| [Some true] -> Lwt.return None
		| _ -> Lwt.return (Some "Unknown user or wrong password")
;;

let set_user_data user_id fname lname new_password =
	Lwt_pool.use db_pool (fun dbh -> if new_password = "" then
		PGSQL(dbh) "UPDATE users \
			SET first_name = $fname, last_name = $lname \
			WHERE id = $user_id"
	else
		PGSQL(dbh) "UPDATE users \
			SET first_name = $fname, last_name = $lname, \
			password = crypt($new_password, gen_salt('md5')) \
		WHERE id = $user_id")
;;

let get_confirmable_attendance term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
	"SELECT first_name, last_name, learning_week, weekday, start_time, end_time \
		FROM attendance a JOIN sessions s ON a.session_id = s.id \
			JOIN timetable t ON s.timetable_id = t.id \
			JOIN users u ON u.id = a.user_id \
		WHERE confirmed = 'W' AND t.term = $term")
;;

let get_planned_sessions user_id term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
	"SELECT MAX(year), gs.week, COUNT(s.id) \
		FROM timetable t JOIN sessions s ON t.id = s.timetable_id \
		JOIN generate_series(1,53) AS gs(week) ON gs.week BETWEEN start_week AND end_week \
		LEFT JOIN optional_sessions os ON t.id = os.timetable_id AND gs.week = os.week \
		JOIN students st ON (st.group_number = t.group_number OR t.group_number IS NULL) \
		WHERE user_id = $user_id AND t.term = $term AND os.week IS NULL \
		AND type IN ('S', 'L') GROUP BY gs.week ORDER BY 1, 2")
;;

let get_user_attendance term week =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
	"SELECT u.id, u.first_name, u.last_name, MAX(st.student_id), COUNT(a.session_id), bool_and(visa) \
		FROM users u JOIN students st ON u.id = st.user_id \
			LEFT JOIN attendance a ON u.id = a.user_id AND a.learning_week = $week \
			LEFT JOIN sessions s ON s.id = a.session_id \
			LEFT JOIN timetable t ON s.timetable_id = t.id AND st.term = t.term \
		WHERE (t.term = $term OR t.term IS NULL) \
		AND ($week BETWEEN joined_week AND left_week OR \
			($week >= joined_week AND left_week IS NULL)) \
		GROUP BY u.id")
;;

let get_approvable_blogs term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
	"SELECT u.id, u.first_name, u.last_name, title, learning_week \
		FROM blogs b JOIN users u ON b.user_id = u.id \
		WHERE term = $term AND NOT b.approved \
		ORDER By learning_week ASC")
;;

let approve_blog term user_id week =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh) 
		"UPDATE blogs SET approved = true \
			WHERE term = $term AND user_id = $user_id AND learning_week = $week")
;;

let get_user_weeks user_id term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT joined_week, left_week \
			FROM users u JOIN students st ON u.id = st.user_id \
			WHERE u.id = $user_id AND term = $term") >>=
	function
	| [] -> Lwt.fail Not_found
	| [jw, lw] -> Lwt.return (jw, lw)
	| _ -> Lwt.fail_with "multiple users found (get_user_weeks)"
;;

let get_students ?active_only:(act=true) term =
	Lwt_pool.use db_pool (fun dbh -> 
		if act then
			PGSQL(dbh) "SELECT u.id, first_name, last_name \
				FROM users u JOIN students st ON u.id = st.user_id \
				WHERE term = $term AND left_week IS NULL"
		else
			PGSQL(dbh) "SELECT u.id, first_name, last_name \
				FROM users u JOIN students st ON u.id = st.user_id \
				WHERE term = $term")
;;

let get_group_info group_number term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT weekday \
			FROM timetable \
			WHERE group_number = $group_number AND term = $term") >>=
	function
	| [] -> Lwt.fail Not_found
	| [wd] -> Lwt.return wd
	| _ -> Lwt.fail_with "multiple timetables for group found"
;;

let get_criteria term =
	Lwt_pool.use db_pool (fun dbh -> PGSQL(dbh)
		"SELECT id, criterion, description \
			FROM presentation_criteria \
			WHERE term = $term \
			ORDER BY id")
;;

let set_presentation_score pres_id scorer_id term crit_id score comment =
	Lwt_pool.use db_pool (fun dbh -> 
		PGSQL(dbh) "INSERT INTO presentation_scores (presenter_id, scorer_id, term, criterion_id, score, comment) \
			VALUES \
			($pres_id, $scorer_id, $term, $crit_id, $score, $comment) \
			ON CONFLICT (presenter_id, scorer_id, term, criterion_id) DO UPDATE SET score = EXCLUDED.score, comment = EXCLUDED.comment")
;;

let set_presentation_tutor_feedback pres_id scorer_id term topic duration grade comments =
	Lwt_pool.use db_pool (fun dbh ->
		PGSQL(dbh) "INSERT INTO presentation_tutor_feedback (presenter_id, scorer_id, term, topic, duration, provisional_grade, comments) \
			VALUES \
			($pres_id, $scorer_id, $term, $topic, $duration, $grade, $comments) \
			ON CONFLICT (presenter_id, scorer_id, term) DO UPDATE SET topic = EXCLUDED.topic, \
				duration = EXCLUDED.duration, provisional_grade = EXCLUDED.provisional_grade, comments = EXCLUDED.comments")
;;

let get_presentation_scores pres_id scorer_id term =
	Lwt_pool.use db_pool (fun dbh ->
		PGSQL(dbh) "SELECT criterion_id, score, comment \
			FROM presentation_scores \
			WHERE presenter_id = $pres_id AND scorer_id = $scorer_id \
			AND term = $term") >>=
	Lwt_list.map_s (fun (c, s, cm) -> Lwt.return (c, (Some s, cm)))
;;

let get_presentation_tutor_feedback pres_id term =
	Lwt_pool.use db_pool (fun dbh ->
		PGSQL(dbh) "SELECT topic, duration, provisional_grade, final_grade, comments \
			FROM presentation_tutor_feedback \
			WHERE presenter_id = $pres_id \
				AND term = $term") >>=
	function
	| [] -> Lwt.fail Not_found
  | [tf] -> Lwt.return tf
	| _ -> Lwt.fail_with "multiple feedback instances found"
;;

let get_presentation_averages pres_id term =
	Lwt_pool.use db_pool (fun dbh ->
		PGSQL(dbh) "SELECT ps.criterion_id, MAX(criterion), ROUND(AVG(score),1) \
			FROM presentation_scores ps \
			JOIN presentation_criteria pc ON ps.criterion_id = pc.id AND ps.term = pc.term \
			WHERE presenter_id = $pres_id AND ps.term = $term \
			GROUP BY ps.criterion_id \
			ORDER BY 1") >>=
	function
	| [] -> Lwt.fail Not_found
	| x -> Lwt.return x
;;

let get_presentation_comments pres_id term =
	let rec zip_comments id cname l ccl res =
		match l with
		| [] -> (cname,ccl)::res
		|	(i,c,cm)::t -> 
			if i = id then zip_comments id cname t (cm::ccl) res 
			else zip_comments i c t [cm] ((cname, ccl)::res) in
	Lwt_pool.use db_pool (fun dbh ->
		PGSQL(dbh) "SELECT criterion_id, criterion, comment \
			FROM presentation_scores ps \
			JOIN presentation_criteria pc ON ps.criterion_id = pc.id AND ps.term = pc.term \
			WHERE presenter_id = $pres_id AND ps.term = $term \
			AND comment <> '' \
			ORDER BY 1"
	) >>= 
	function
	| [] -> Lwt.return []
	| (i, c, cm)::t -> Lwt.return (zip_comments i c t [cm] [])
;;

let get_report_scores ?(unpublished=false) user_id term =
	Lwt_pool.use db_pool (fun dbh ->
		if unpublished then
			PGSQL(dbh) "SELECT published, quality_score, quality_feedback, independence_score, independence_feedback, communication_score, communication_feedback \
				FROM report_scores \
				WHERE user_id = $user_id AND term = $term"
		else
			PGSQL(dbh) "SELECT published, quality_score, quality_feedback, independence_score, independence_feedback, communication_score, communication_feedback \
				FROM report_scores \
				WHERE user_id = $user_id AND term = $term AND published"
	) >>=
	function
	| [] -> Lwt.fail Not_found
	| [x] -> Lwt.return x
	| _ -> Lwt.fail_with "multiple feedback instances found"
;;

let set_report_scores user_id term pub qs qf ids idf cs cf =
	Lwt_pool.use db_pool (fun dbh ->
		PGSQL(dbh) "INSERT INTO report_scores \
			(user_id, term, published, quality_score, quality_feedback, independence_score, independence_feedback, communication_score, communication_feedback) \
			VALUES
			($user_id, $term, $pub, $qs, $qf, $ids, $idf, $cs, $cf) \
			ON CONFLICT (user_id, term) DO UPDATE \
				SET published = EXCLUDED.published, \
				quality_score = EXCLUDED.quality_score, \
				quality_feedback = EXCLUDED.quality_feedback, \
				independence_score = EXCLUDED.independence_score, \
				independence_feedback = EXCLUDED.independence_feedback, \
				communication_score = EXCLUDED.communication_score, \
				communication_feedback = EXCLUDED.communication_feedback"
	)
;;

let get_student_info user_id term =
	Lwt_pool.use db_pool (fun dbh ->
		PGSQL(dbh) "SELECT student_id \
			FROM students \
			WHERE user_id = $user_id AND term = $term"
	) >>=
	function
	| [] -> Lwt.fail Not_found
	| [id] -> Lwt.return id
	| _ -> Lwt.fail_with "multiple students found"
;;
