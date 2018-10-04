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

(* Utility functions *)

let%shared schedule_table ayear gnr weekday =
	let sw = ~%(!Moab_config.presentation_start_week) in
	let%lwt schedule = get_schedule (ayear, gnr) >|= drop (sw - 1) in
	let%lwt group_members = Moab_students.get_students (ayear, Some gnr) in
	let%lwt learning_weeks = Moab_terms.get_learning_weeks ayear >|= drop (sw - 1) in
	let%lwt trs = map2i_s (fun i (week, uid1, uid2) (w, y) ->
		let create_field uid = match uid with
		| None -> 
				if (2*i) < List.length group_members then
					Lwt.return @@
					td ~a:[a_class ["available"]] [pcdata [%i18n S.available]]
				else
					Lwt.return @@
					td ~a:[a_class ["not-available"]] [pcdata [%i18n S.not_available]]
		| Some u -> let%lwt (fn, ln) = Moab_users.get_name u in
			Lwt.return @@ td [pcdata fn; pcdata " "; pcdata ln] in
		let%lwt f1 = create_field uid1 in
		let%lwt f2 = create_field uid2 in
		let (d1, _) = Date.week_first_last (Int32.to_int w) y in
		let d = Date.add d1 (Date.Period.day weekday) in
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
	| Some g -> let%lwt schedule_table = schedule_table ayear g weekday in
			Moab_container.page (Some myid) [
				div ~a:[a_class ["content-box"]] [
					h1 [pcdata [%i18n S.schedule_presentation]];
					schedule_table
				]
			]

let%server schedule_presentation_handler myid () () =
	real_schedule_presentation_handler myid () ()

let%client schedule_presentation_handler =
	real_schedule_presentation_handler
