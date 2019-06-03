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
					Lwt.return @@ tr [
						td [txt (Printf.sprintf "%s %s" fn ln)];
						td [txt sid];
						td [txt (Printf.sprintf "%.1f" pres_peer)];
						td [txt (default [%i18n S.tbd] pres_tutor)];
						td [txt (Printf.sprintf "%.1f" pres_total)];
						td [txt (string_of_int blogs)];
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
			table 
			(tr [th [txt [%i18n S.name]]; th [txt [%i18n S.student_id]]]::trs)
		]
	| _ -> Moab_container.page (Some myid) []
	
