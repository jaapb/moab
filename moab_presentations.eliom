[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
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
]

(* Database access *)

let%server get_schedule (ayear, gnr) =
	Moab_presentation_db.get_schedule ayear gnr

let%client get_schedule =
	~%(Eliom_client.server_function [%derive.json: string * int]
			(Os_session.connected_wrapper get_schedule))

(* Utility functions *)

let%shared schedule_table ayear gnr weekday =
	let%lwt schedule = get_schedule (ayear, gnr) in
	let%lwt learning_weeks = Moab_terms.get_learning_weeks ayear in
	Ocsigen_messages.console (fun () -> Printf.sprintf "s: %d lw: %d" (List.length schedule) (List.length learning_weeks));
	let%lwt trs = map2_s (fun (week, uid1, uid2) (w, y) ->
		let create_field uid = match uid with
		| None -> Lwt.return [pcdata [%i18n S.none]]
		| Some u -> let%lwt (fn, ln) = Moab_users.get_name u in
			Lwt.return [pcdata fn; pcdata " "; pcdata ln] in
		let%lwt f1 = create_field uid1 in
		let%lwt f2 = create_field uid2 in
		let (d1, d2) = Date.week_first_last (Int32.to_int w) y in
		Lwt.return @@ tr [
			td [
				pcdata (Printer.Date.sprint "%b %d" d1);
				pcdata " - ";
				pcdata (Printer.Date.sprint "%b %d" d2)
			];
			td f1;
			td f2
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
	match gnr with
	| None -> Moab_container.page (Some myid) [p [pcdata [%i18n S.no_group_number]]]
	| Some g -> let%lwt schedule_table = schedule_table ayear g 1 in
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
