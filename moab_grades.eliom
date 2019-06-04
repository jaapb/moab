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
	| Admin | Examiner -> 
		let ayear = ~%(!Moab_config.current_academic_year) in
		let%lwt trs = Moab_students.get_students (ayear, None, Some 24) >>=
			Lwt_list.map_p (fun uid ->
				let%lwt (fn, ln) = Moab_users.get_name uid in
				let%lwt sid = Moab_students.get_student_id uid in
				try%lwt
					let%lwt pres_peer = Moab_presentations.get_average_scores_opt (ayear, uid) >>=
						function
						| None -> Lwt.return_none
						| Some s -> Lwt_list.fold_left_s (fun acc (_, s) -> Lwt.return (s +. acc)) 0.0 s >>= Lwt.return_some in
					let%lwt (_, duration, _, pres_tutor, _) = Moab_presentations.get_admin_scores (ayear, uid) in
					let pres_total = compute_pres_total pres_peer duration pres_tutor in
					let%lwt blogs = Moab_blogs.get_nr_blogs (uid, ayear, true) >|= (fun x -> max 0 ((Int64.to_int x) - 14)) in
					let%lwt feedback = Moab_reports.get_report_feedback_opt (ayear, uid) in
					let total_report = match feedback with
						| None -> None
						| Some (_, q, _, i, _, g) -> Some (q + i + g) in
					let full_total = match pres_total, total_report with
						| None, _ | _, None -> None
						| Some p, Some r -> Some (p +. (float_of_int blogs) +. (float_of_int r)) in
					Lwt.return @@ tr ([
						td [txt (Printf.sprintf "%s %s" fn ln)];
						td [txt sid];
						td ~a:[a_class ["presentation"]] [txt (match pres_peer with None -> "--" | Some p -> Printf.sprintf "%.1f" p)];
						td ~a:[a_class ["presentation"]] [txt (default "--" pres_tutor)];
						td ~a:[a_class ["presentation"]] [txt (string_of_int duration)];
						td ~a:[a_class ["presentation"]] [b [match pres_total with
							| None -> txt "--"
							| Some t -> a ~service:Moab_services.view_presentation_feedback_service
									[txt (Printf.sprintf "%.1f" t)]
									(Some uid)
						]];
						td ~a:[a_class ["blog"]]  [b [txt (string_of_int blogs)]]
					] @
					(match feedback with
					| None -> [td ~a:[a_colspan 3; a_class ["report"]] [txt "--"]]
					| Some (_, qg, _, ing, _, cg) -> [
							td ~a:[a_class ["report"]] [txt (string_of_int qg)];
							td ~a:[a_class ["report"]] [txt (string_of_int ing)];
							td ~a:[a_class ["report"]] [txt (string_of_int cg)]
					]) @ [
						td ~a:[a_class ["report"]] [b [match total_report with
							| None -> txt "--"
							| Some t -> a ~service:Moab_services.view_report_feedback_service
									[txt (string_of_int t)]
									(Some uid)
						]];	
						td [txt (match full_total with None -> "--" | Some t -> Printf.sprintf "%.1f" t)];
						let (g, c) = to_20point full_total in
							td ~a:[a_class [c]] [b [txt (string_of_int g)]]
					])
				with
				| Not_found -> Lwt.return @@ tr [
						td [txt (Printf.sprintf "%s %s" fn ln)];
						td [txt sid];
						td ~a:[a_colspan 11] [txt [%i18n S.results_not_found]]
					]
			)
		in
		Moab_container.page (Some myid)
		[
			p [txt [%i18n S.grades_explanation]];
			table ~a:[a_class ["grades-table"]]
			(tr [
				th ~a:[a_colspan 2] [];
				th ~a:[a_colspan 4; a_class ["presentation"]] [txt [%i18n S.presentation]];
				th [];	
				th ~a:[a_colspan 4; a_class ["report"]] [txt [%i18n S.report]];
				th ~a:[a_colspan 2] []
			 ]::tr [
				th [txt [%i18n S.name]];
				th [txt [%i18n S.student_id]];
				th ~a:[a_class ["presentation"]] [txt [%i18n S.peer]];
				th ~a:[a_class ["presentation"]] [txt [%i18n S.tutor]];
				th ~a:[a_class ["presentation"]] [txt [%i18n S.duration]];
				th ~a:[a_class ["presentation"]] [txt [%i18n S.total]];
				th ~a:[a_class ["blog"]] [txt [%i18n S.blog]];
				th ~a:[a_class ["report"]] [txt [%i18n S.quality]];
				th ~a:[a_class ["report"]] [txt [%i18n S.independence]];
				th ~a:[a_class ["report"]] [txt [%i18n S.communication]];
				th ~a:[a_class ["report"]] [txt [%i18n S.total]];
				th [txt [%i18n S.total]];
				th [txt [%i18n S.twenty_point]]
			]::trs)
		]
	| _ -> Moab_container.page (Some myid) [
			div ~a:[a_class ["content-box"]] [
				h1 [txt [%i18n S.error ~capitalize:true]];
       p [txt [%i18n S.not_admin_or_examiner]]
			]]
			
	
