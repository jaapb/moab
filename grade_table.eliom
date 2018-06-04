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

let make_row id fn ln =
	Lwt.catch (fun () ->
		let%lwt (student_id, fbe) = Moab_db.get_student_info id !Moab.term in
		let%lwt (pres_peer, pres_tutor, pres_dur) = Lwt.catch (fun () ->
			let%lwt pres_sc = Moab_db.get_presentation_averages id !Moab.term in
			let%lwt (_, duration, _, fgrade, _) = Moab_db.get_presentation_tutor_feedback id !term in
	 		Lwt.return (Some (List.fold_left (fun acc (_, _, s) -> acc +. (float_of_string (Moab_utils.default "" s))) 0.0 pres_sc),
				fgrade, duration)
		)
		(function 
		| Not_found -> Lwt.return (None, None, 0)
		| e -> Lwt.fail e) in
		let%lwt fg = Moab_db.get_feedback_given id !Moab.term 24 in
		let%lwt fp = Moab_db.get_feedback_possible id !Moab.term 24 in
		let fb_perc = List.length fg * 100 / List.length fp in
		let pres = Moab_utils.calculate_pres pres_peer pres_tutor pres_dur fb_perc fbe in
		let%lwt blog_grade = Lwt.catch (fun () ->
			let%lwt blog = Moab_db.get_user_blogs id !Moab.term in
			let%lwt (jw, _) = Moab_db.get_user_weeks id !Moab.term in
    	let nr = List.length (List.filter (fun (_, a) -> a) blog) in
    	Lwt.return (max 0 (nr - 14 + jw - 1))
		) 
		(function
		| Not_found -> Lwt.return 0
		| e -> Lwt.fail e) in
		let%lwt (qg, idg, cg) = Lwt.catch (fun () ->
			let%lwt (_, q, _, i, _, c, _) = Moab_db.get_report_scores ~unpublished:true id !Moab.term in
			Lwt.return (Some q, Some i, Some c)
		)
		(function
		| Not_found -> Lwt.return (None, None, None)
		| e -> Lwt.fail e) in	
		let project_grade = Moab_utils.calculate_project qg idg cg in
		let total = Moab_utils.calculate_total pres blog_grade project_grade in
		let tp = Moab_utils.twenty_point_grade total in
			Lwt.return ( 
				Printf.sprintf "%s %s" fn ln,
				id,
				Some (student_id,
				pres_peer,
				pres_tutor,
				pres_dur,
				fb_perc,
				fbe,
				pres,
				blog_grade,
				qg,
				idg,
				cg,	
				project_grade,
				total,
				tp
			))
	)
	(function
	| Not_found -> Lwt.return (
			Printf.sprintf "%s %s" fn ln,
			id,
			None
		)
	| e -> Lwt.fail e
	)
;;

let display_row tc =
	let tcs = List.sort (fun (_, _, x) (_, _, y) -> match x, y with
		| None, None -> 0
		| _, None -> 2
		| None, _ -> -1
		| Some (_, _, _, _, _, _, _, _, _, _, _, _, _, x'), Some (_, _, _, _, _, _, _, _, _, _, _, _, _, y') -> compare x' y') tc in
	List.map (fun (name, id, x) ->
		match x with
		| None ->
				tr [
					td [pcdata "name"];
					td [pcdata id];
					td ~a:[a_colspan 14] [pcdata "Data missing"]
				]
		| Some (student_id, pres_peer, pres_tutor, pres_dur, fb_perc, fbe, pres, blog_grade, qg, idg, cg, project_grade, total, tp) ->
				tr [
					td [pcdata name];
					td [a ~service:view_feedback_service [pcdata id] (Some id)];
					td [pcdata student_id];
					td [match pres_peer with | None -> b [pcdata "NONE"] | Some x -> pcdata (Printf.sprintf "%.1f" x)];
					td [match pres_tutor with | None -> b [pcdata "NONE"] | Some x -> pcdata (Printf.sprintf "%.1f" x)];
					td [pcdata (Printf.sprintf "%d" pres_dur)];
					td [pcdata (Printf.sprintf "%d" fb_perc)];
					td [pcdata (if fbe then "YES" else "")];
					td ~a:[a_class ["presentation"]] [match pres with None -> b [pcdata "NONE"] | Some x -> pcdata (Printf.sprintf "%.1f" x)];
					td ~a:[a_class ["blog"]] [pcdata (Printf.sprintf "%d" blog_grade)]; 
					td [match qg with None -> b [pcdata "NONE"] | Some x -> pcdata (Printf.sprintf "%ld" x)];
					td [match idg with None -> b [pcdata "NONE"] | Some x -> pcdata (Printf.sprintf "%ld" x)];
					td [match cg with None -> b [pcdata "NONE"] | Some x -> pcdata (Printf.sprintf "%ld" x)];
					td ~a:[a_class ["project"]] [match project_grade with None -> b [pcdata "NONE"] | Some x -> pcdata (Printf.sprintf "%ld" x)];
					td ~a:[a_class ["total"]] [match total with None -> b [pcdata "NONE"] | Some x -> pcdata (Printf.sprintf "%.1f" x)];
					td ~a:[a_class ["twenty"]] [pcdata (Printf.sprintf "%d" tp)]
				]
	) tcs
;;

let grade_table_page () () =
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, _, _, is_admin) -> 
		if not is_admin then
			error_page "You are not an administrator."
		else
		let%lwt st = Moab_db.get_students !Moab.term in
		Lwt.catch (fun () ->
			let%lwt table_contents =
			Lwt_list.map_s (fun (id, fn, ln) ->
				make_row id fn ln
      ) st in
			container
			[
				h1 [pcdata "Grade table"];
				table ~a:[a_class ["grade_table"]] 
				(tr [	
					th [pcdata "Name"];
					th [pcdata "User ID"];
					th [pcdata "Student ID"];
					th [pcdata "Pres peer"];
					th [pcdata "Pres tutor"];
					th [pcdata "Pres dur"];
					th [pcdata "Pres FB"];
					th [pcdata "Pres exemp"];
					th [pcdata "Presentation"];
					th [pcdata "Blog"];
					th [pcdata "Project Q"];
					th [pcdata "Project I"];
					th [pcdata "Project C"];
					th [pcdata "Project"];
					th [pcdata "Total"];
					th [pcdata "20-point"]
         ]::
				 display_row table_contents
				)
			]
		)
		(function
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Eliom_registration.Any.register ~service:grade_table_service grade_table_page;
;;
