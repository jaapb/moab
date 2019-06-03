[%%shared
	open Eliom_parameter
	open CalendarLib
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Lwt.Infix
	open Moab_base
]

(* Local types *)

(* Local services *)

(* Database access *)

(* Widgets *)

(* Utility functions *)

let%shared to_20point f =
	if Float.compare f 78.5 >= 0 then 1
	else if Float.compare f 75.5 >= 0 then 2
	else if Float.compare f 72.5 >= 0 then 3
	else if Float.compare f 69.5 >= 0 then 4
	else if Float.compare f 66.5 >= 0 then 5
	else if Float.compare f 64.5 >= 0 then 6
	else if Float.compare f 61.5 >= 0 then 7
	else if Float.compare f 59.5 >= 0 then 8
	else if Float.compare f 56.5 >= 0 then 9
	else if Float.compare f 54.5 >= 0 then 10
	else if Float.compare f 51.5 >= 0 then 11
	else if Float.compare f 49.5 >= 0 then 12
	else if Float.compare f 46.5 >= 0 then 13
	else if Float.compare f 44.5 >= 0 then 14
	else if Float.compare f 41.5 >= 0 then 15
	else if Float.compare f 39.5 >= 0 then 16
	else if Float.compare f 34.5 >= 0 then 17
	else if Float.compare f 29.5 >= 0 then 18
	else 19

(* Handlers *)

let%shared view_grades_handler myid () () =
	let%lwt t = Moab_users.get_user_type myid in
	match t with
	| Admin -> 
		let ayear = ~%(!Moab_config.current_academic_year) in
		let%lwt trs = Moab_students.get_students (ayear, None, Some 24) >>=
			Lwt_list.map_p (fun uid ->
				let%lwt (fn, ln) = Moab_users.get_name uid in
				let%lwt sid = Moab_students.get_student_id uid in
				try%lwt
					let%lwt pres_peer = Moab_presentations.get_average_scores (ayear, uid) >>=
						Lwt_list.fold_left_s (fun acc (_, s) -> Lwt.return (s +. acc)) 0.0 in
					let%lwt (_, _, _, pres_tutor, _) = Moab_presentations.get_admin_scores (ayear, uid) in
					let pt_float = match pres_tutor with None -> 0.0 | Some t -> float_of_string t in
					let pres_total = pres_peer +. pt_float in
					let%lwt blogs = Moab_blogs.get_nr_blogs (uid, ayear, true) >|= (fun x -> max 0 ((Int64.to_int x) - 14)) in
					let%lwt (_, qg, _, ing, _, cg) = Moab_reports.get_report_feedback (ayear, uid) in
					let total_report = qg + ing + cg in
					let full_total = pres_total +. (float_of_int blogs) +. (float_of_int total_report) in
					Lwt.return @@ tr [
						td [txt (Printf.sprintf "%s %s" fn ln)];
						td [txt sid];
						td [txt (Printf.sprintf "%.1f" pres_peer)];
						td [txt (default [%i18n S.tbd] pres_tutor)];
						td [txt (Printf.sprintf "%.1f" pres_total)];
						td [txt (string_of_int blogs)];
						td [txt (string_of_int qg)];
						td [txt (string_of_int ing)];
						td [txt (string_of_int cg)];
						td [txt (string_of_int total_report)];
						td [txt (Printf.sprintf "%.1f" full_total)];
						td [b [txt (string_of_int (to_20point full_total))]]
					]
				with
				| Not_found -> Lwt.return @@ tr [
						td [txt (Printf.sprintf "%s %s" fn ln)];
						td [txt sid];
						td ~a:[a_colspan 2] [txt [%i18n S.results_not_found]]
					]
			)
		in
		Moab_container.page (Some myid)
		[
			table ~a:[a_class ["grades-table"]]
			(tr [th [txt [%i18n S.name]]; th [txt [%i18n S.student_id]]]::trs)
		]
	| _ -> Moab_container.page (Some myid) []
	
