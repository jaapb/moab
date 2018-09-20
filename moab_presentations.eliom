[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
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
	let%lwt trs = Lwt_list.map_s (fun (week, uid1, uid2) ->
		Lwt.return @@ tr [
			td [pcdata (Int32.to_string week)];
			td [pcdata (match uid1 with None -> [%i18n S.none] | Some u -> Int64.to_string u)];
			td [pcdata (match uid2 with None -> [%i18n S.none] | Some u -> Int64.to_string u)];
		]) schedule in
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
