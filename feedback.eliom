[%%shared
	open Eliom_lib
	open Eliom_content
	open Html.D
	open Eliom_service
	open Eliom_parameter
]

[%%server
	open Services
	open Moab
	open CalendarLib
]

let feedback_values = Eliom_reference.eref ~scope:Eliom_common.request_scope None;;
let feedback_err = Eliom_reference.eref ~scope:Eliom_common.request_scope None;;

exception No_score of int32

let no_session_found uid =
	Moab_db.log uid (Eliom_request_info.get_remote_ip ()) `No_session_found >>=
	fun () -> container [
		h1 [pcdata "No session found"];
		p [pcdata "No session was found at this time. This action has been logged."]
	]
;;

let do_feedback_page () (pres_id, scores) =
	Eliom_reference.set feedback_values (Some (pres_id, scores)) >>=
	fun () -> Eliom_reference.get user >>=
	fun s -> match s with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (s_id, _, _, _) -> begin
		match pres_id with
		| None -> 
				Eliom_reference.set feedback_err (Some "No presenter selected");
				Eliom_registration.Action.send ()
		| Some uid -> Lwt.catch (fun () ->
				Lwt_list.iter_s (fun (c_id, (s, comment)) ->
					match s with
					| None ->
							Eliom_reference.set feedback_err (Some (Printf.sprintf "No score for criterion %ld\n" c_id));
							Lwt.fail (No_score c_id)
					| Some score -> Moab_db.set_presentation_score uid s_id !term c_id score comment
				) scores >>=
				fun () -> Eliom_reference.set feedback_values None >>=
				fun () -> Eliom_registration.Redirection.send (Eliom_registration.Redirection main_service)
			)
			(function
			| No_score _ -> Eliom_registration.Action.send ()
			| e -> Eliom_registration.Action.send ())
		end
;;

let feedback_page () () =
	let zip_crits c s =
		match c, s with
		| c', [] -> List.map (fun (id1, crit, descr) -> (id1, crit, descr, None, "")) c'
		| [], s' -> raise (Failure "no criteria detected")
		| c', s' -> List.map2 (fun (id1, crit, descr) (id2, (score, comment)) ->
				if id1 = id2 then
					(id1, crit, descr, score, comment)
				else
					raise (Failure (Printf.sprintf "zip_crits %ld %ld" id1 id2))
			) (List.sort (fun (i1, _, _) (i2, _, _) -> compare i1 i2) c') (List.sort (fun (i1, (_, _)) (i2, (_, _)) -> compare i1 i2) s')
	in
	let do_feedback_service = create_attached_post ~fallback:feedback_service
		~post_params:(radio string "presenter" ** list "scores" (int32 "criterion_id" ** radio int "score" ** string "comment")) () in
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:do_feedback_service do_feedback_page;
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, _, _, _) -> 
		Lwt.catch (fun () ->
			let week = Date.week (Date.today ()) in
			let%lwt (session_id, session_type, sgroup) = Moab_db.find_sessions_now () in
			let%lwt (group, _, _) = Moab_db.get_user_group uid !Moab.term in
			let%lwt this_lw = Moab_db.current_learning_week group !Moab.term in
			let%lwt (p1, p2) = match this_lw with
			| None -> Lwt.return (None, None)
			| Some lw -> Moab_db.get_presenters !term group lw in
			let%lwt crits = Moab_db.get_criteria !term in
			let%lwt x = Eliom_reference.get feedback_values in
			let (set_pres_id, set_scores) = match x with
			| None -> None, []
			| Some (None, l) -> None, l 
			| Some (Some p, l) -> Some p, l  in
			let%lwt err = Eliom_reference.get feedback_err in
			Eliom_reference.set feedback_values None >>=
			fun () -> Eliom_reference.set feedback_err None >>=
			fun () -> let crits_plus_values = zip_crits crits set_scores in
			match session_type with
			| `No_session -> no_session_found uid
			| `Lecture | `Test ->
				container
				[	
					h1 [pcdata "Lecture"];
					p [pcdata "The session currently running is a lecture, no feedback required."]
				]
			| `Seminar -> 
				container
				[
					h1 [pcdata "Feedback"];
					p ~a:[a_class ["error"]] (match err with None -> [] | Some e -> [pcdata e]);
					Form.post_form ~service:do_feedback_service (fun (presenter_id, scores) -> [
						table ~a:[a_class ["feedback_table"]] (
							tr [
								th [pcdata "Presenter: "];
								td ~a:[a_colspan 7] (match p1, p2 with
								| None, None ->
									[i [pcdata "no presentations scheduled"]]
								| Some (i1, fn1, ln1), None when i1 = uid ->
									[i [pcdata "Yourself"]]
								| None, Some (i2, fn2, ln2) when i2 = uid ->
									[i [pcdata "Yourself"]]
								| Some (i1, fn1, ln1), None ->
									[Form.radio ~checked:(Some i1 = set_pres_id) ~name:presenter_id ~value:i1 Form.string; pcdata " "; pcdata (Printf.sprintf "%s %s" fn1 ln1)]
								| Some (i1, fn1, ln1), Some (i2, fn2, ln2) when i2 = uid ->
									[Form.radio ~checked:(Some i1 = set_pres_id) ~name:presenter_id ~value:i1 Form.string; pcdata " "; pcdata (Printf.sprintf "%s %s" fn1 ln1)]
								| None, Some (i2, fn2, ln2) ->
									[Form.radio ~checked:(Some i2 = set_pres_id) ~name:presenter_id ~value:i2 Form.string; pcdata " "; pcdata (Printf.sprintf "%s %s" fn2 ln2)]
								| Some (i1, fn1, ln1), Some (i2, fn2, ln2) when i1 = uid ->
									[Form.radio ~checked:(Some i1 = set_pres_id) ~name:presenter_id ~value:i2 Form.string; pcdata " "; pcdata (Printf.sprintf "%s %s" fn2 ln2)]
								| Some (i1, fn1, ln1), Some (i2, fn2, ln2) ->
									[Form.radio ~checked:(Some i1 = set_pres_id) ~name:presenter_id ~value:i1 Form.string; pcdata " "; pcdata (Printf.sprintf "%s %s" fn1 ln1);
									pcdata " ";
									Form.radio ~checked:(Some i2 = set_pres_id) ~name:presenter_id ~value:i2 Form.string; pcdata " "; pcdata (Printf.sprintf "%s %s" fn2 ln2)]
								)
							]::
							tr ~a:[a_class ["grade_descriptions"]] [
								td [];
								td ~a:[a_class ["grade_button"]] [pcdata "Nonexistent"];
								td ~a:[a_class ["grade_button"]] [pcdata "Poor"];
								td ~a:[a_class ["grade_button"]] [pcdata "Barely sufficient"];
								td ~a:[a_class ["grade_button"]] [pcdata "OK"];
								td ~a:[a_class ["grade_button"]] [pcdata "Good"];
								td ~a:[a_class ["grade_button"]] [pcdata "Excellent"];
								td []
							]::
							tr ~a:[a_class ["grades"]] [
								td [i [pcdata "Criteria"]];
								th ~a:[a_class ["grade_button"]] [pcdata "0"];
								th ~a:[a_class ["grade_button"]] [pcdata "1"];
								th ~a:[a_class ["grade_button"]] [pcdata "2"];
								th ~a:[a_class ["grade_button"]] [pcdata "3"];
								th ~a:[a_class ["grade_button"]] [pcdata "4"];
								th ~a:[a_class ["grade_button"]] [pcdata "5"];
								th [pcdata "Comment"]
							]::
							scores.it (fun (p_crit_id, (p_score, p_comment)) (id, crit, descr, score, comment) init ->
								tr [
									th [
										Form.input ~input_type:`Hidden ~name:p_crit_id ~value:id Form.int32;
										pcdata crit];
									td ~a:[a_class ["grade0"; "grade_button"]; a_rowspan 2] [
										Form.radio ~checked:(score = Some 0) ~name:p_score ~value:0 Form.int
									];
									td ~a:[a_class ["grade1"; "grade_button"]; a_rowspan 2] [
										Form.radio ~checked:(score = Some 1) ~name:p_score ~value:1 Form.int
									];
									td ~a:[a_class ["grade2"; "grade_button"]; a_rowspan 2] [
										Form.radio ~checked:(score = Some 2) ~name:p_score ~value:2 Form.int
									];
									td ~a:[a_class ["grade3"; "grade_button"]; a_rowspan 2] [
										Form.radio ~checked:(score = Some 3) ~name:p_score ~value:3 Form.int
									];
									td ~a:[a_class ["grade4"; "grade_button"]; a_rowspan 2] [
										Form.radio ~checked:(score = Some 4) ~name:p_score ~value:4 Form.int
									];
									td ~a:[a_class ["grade5"; "grade_button"]; a_rowspan 2] [
										Form.radio ~checked:(score = Some 5) ~name:p_score ~value:5 Form.int
									];
									td ~a:[a_rowspan 2] [
										Form.input ~input_type:`Text ~name:p_comment ~value:comment Form.string
									]
								]::
								tr [
									th ~a:[a_class ["description"]] [pcdata (match descr with None -> "" | Some x -> x)]
								]::
								init
							) crits_plus_values
							[tr [
								td ~a:[a_colspan 8] [Form.input ~input_type:`Submit ~value:"Submit" Form.string; pcdata " (also registers attendance)"]
							]]
 						)
					]) ()
				]
		)
		(function
		| Failure s -> 
			container
			[
				h1 [pcdata "Error"];
				p [pcdata (Printf.sprintf "There was an inconsistency in the database (message: %s)." s)]
			]
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Eliom_registration.Any.register ~service:feedback_service feedback_page;
;;
