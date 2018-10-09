[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
	open Lwt
]

[%%shared

let rec map2_s (f: 'a -> 'b -> 'c Lwt.t) (l1: 'a list) (l2: 'b list): 'c list Lwt.t =
	match l1, l2 with
	| [], [] -> Lwt.return []
	| h1::t1, h2::t2 ->
		let%lwt h = f h1 h2 in
		let%lwt r = map2_s f t1 t2 in
		Lwt.return @@ h::r
	| _, _ -> Lwt.fail (Invalid_argument "map2_s")

let map2i_s (f: int -> 'a -> 'b -> 'c Lwt.t) (l1: 'a list) (l2: 'b list): 'c list Lwt.t =
	let rec map2i_s_aux f c l1 l2 =
		match l1, l2 with
		| [], [] -> Lwt.return []
		| h1::t1, h2::t2 ->
			let%lwt h = f c h1 h2 in
			let%lwt r = map2i_s_aux f (c+1) t1 t2 in
			Lwt.return @@ h::r
		| _, _ -> Lwt.fail (Invalid_argument "map2i_s") in
	map2i_s_aux f 0 l1 l2

let rec take n l =
	match l with
	| [] -> []
	| h::t -> if n = 0 then [] else h::take (n-1) t

let rec drop n l =
	match l with
	| [] -> []
	| h::t -> if n = 0 then l else drop (n-1) t
]

(* Database access *)

let%server get_schedule (ayear, gnr) =
	Moab_presentation_db.get_schedule ayear gnr

let%client get_schedule =
	~%(Eliom_client.server_function [%derive.json: string * int]
			(Os_session.connected_wrapper get_schedule))

let%server schedule_presentation (ayear, learning_week, first, userid) =
	Moab_presentation_db.schedule_presentation ayear learning_week first userid

let%client schedule_presentation =
	~%(Eliom_client.server_function [%derive.json: string * int * bool * int64]
			(Os_session.connected_wrapper schedule_presentation))

let%server find_presentation (ayear, userid) =
	Moab_presentation_db.find_presentation ayear userid

let%client find_presentation =
	~%(Eliom_client.server_function [%derive.json: string * int64]
			(Os_session.connected_wrapper find_presentation))

let%server find_presentation_opt (ayear, userid) =
	try%lwt
		let%lwt x = find_presentation (ayear, userid) in
		Lwt.return_some x
	with
		Not_found -> Lwt.return_none

let%client find_presentation_opt =
	~%(Eliom_client.server_function [%derive.json: string * int64]
			(Os_session.connected_wrapper find_presentation_opt))

(* Utility functions *)

let%shared schedule_table myid ayear gnr weekday =
	let sw = ~%(!Moab_config.presentation_start_week) in
	let%lwt schedule = get_schedule (ayear, gnr) >|= drop (sw - 1) in
	let%lwt group_members = Moab_students.get_students (ayear, Some gnr) in
	let%lwt learning_weeks = Moab_terms.get_learning_weeks ayear >|= drop (sw - 1) in
	let av_clicked = [%client fun ev -> 
		Js.Opt.case (ev##.target)
			(fun () -> ())
			(fun e -> Scanf.sscanf (Js.to_string (e##.id)) "%d-%s" (fun lw order ->
				Lwt.async (fun () ->
					let%lwt date = Moab_terms.date_of_learning_week ~%ayear lw (Date.day_of_int ~%weekday) in
					let%lwt ok = Ot_popup.confirm [
							p [
								pcdata [%i18n S.schedule_message1];
								pcdata " ";
								pcdata (Printer.Date.sprint "%-d %B %Y" date);
								pcdata "."
							];
							p [
								pcdata [%i18n S.schedule_message2]
							]
						]
						[pcdata [%i18n S.confirm ~capitalize:true]] 
						[pcdata [%i18n S.cancel ~capitalize:true]] in
					if ok then
						let%lwt () = schedule_presentation (~%ayear, lw, order = "first", ~%myid) in
						Os_lib.reload ()
					else
						Lwt.return_unit)
			)
		)
	] in
	let%lwt trs = map2i_s (fun i (week, uid1, uid2) (_, w, y) ->
		let id_string lw f =
			Printf.sprintf "%d%s" lw (if f then "-first" else "-second") in
		let create_field first uid = match uid with
		| None -> 
				if (2*i) < List.length group_members then
					Lwt.return @@
					td ~a:[a_class ["available"]; a_onclick av_clicked; a_id (id_string (i+sw) first)]
						[pcdata [%i18n S.available]]
				else
					Lwt.return @@
					td ~a:[a_class ["not-available"]] [pcdata [%i18n S.not_available]]
		| Some u -> let%lwt (fn, ln) = Moab_users.get_name u in
			Lwt.return @@ td [pcdata fn; pcdata " "; pcdata ln] in
		let%lwt f1 = create_field true uid1 in
		let%lwt f2 = create_field false uid2 in
		let (d1, _) = Date.week_first_last (Int32.to_int w) y in
		let d = Date.add d1 (Date.Period.day (weekday - 1)) in
		Lwt.return @@ tr [
			td [
				pcdata (Printer.Date.sprint "%b %d" d);
			];
			f1;
			f2
		]) schedule learning_weeks in
	Lwt.return @@ table ~a:[a_class ["schedule-table"]] (
		tr [
			th [pcdata [%i18n S.date]];
			th [pcdata [%i18n S.first_presenter]];
			th [pcdata [%i18n S.second_presenter]]
		]::
		trs
	)

(* Handlers *)

let%server do_schedule_presentation () () =
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared real_schedule_presentation_handler myid () () =
	let ayear = ~%(!Moab_config.current_academic_year) in
	let%lwt gnr = Moab_students.get_group_number (ayear, myid) in
	let%lwt sids = Moab_sessions.(find_sessions (ayear, Seminar, gnr)) in
	let%lwt weekday = Moab_sessions.get_session_weekday (List.hd sids) in
	match gnr with
	| None -> Moab_container.page (Some myid) [p [pcdata [%i18n S.no_group_number]]]
	| Some g -> let%lwt schedule_table = schedule_table myid ayear g weekday in
			Moab_container.page (Some myid) [
				div ~a:[a_class ["content-box"]] [
					h1 [pcdata [%i18n S.schedule_presentation]];
					p [pcdata [%i18n S.schedule_message]];
					schedule_table
				]
			]

let%server schedule_presentation_handler myid () () =
	real_schedule_presentation_handler myid () ()

let%client schedule_presentation_handler =
	real_schedule_presentation_handler
