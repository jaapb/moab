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

let generate_schedule_page (group_number) () =
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, _, _, is_admin) -> 
		Lwt.catch (fun () ->
			let term = !Moab.term in
			let start_week = !Moab.start_week in
			let%lwt slots = Moab_db.get_presentation_slots term group_number start_week in
			let%lwt weeks = Moab_db.get_learning_weeks group_number term in
			let%lwt wd = Moab_db.get_group_info group_number term in
			container
			[
				h1 [pcdata "Presentation schedule"];
				table
				(
					tr [th [pcdata "Learning week"]; th [pcdata "Date"]; th [pcdata "Presenter 1"]; th [pcdata "Presenter 2"]]::
					(List.map2 (fun (lw, i1, fn1, ln1, i2, fn2, ln2) (w, y) ->
						let (sd, _) = Date.week_first_last w y in
						let day = Date.add sd (Date.Period.day (wd - 1)) in
						let n1s = match i1, fn1, ln1 with
						| Some i, Some fn, Some ln ->
							let n = Printf.sprintf "%s %s" fn ln in
							if i = uid then b [pcdata n] else pcdata n
						| _ -> i [pcdata "no presenter"] in
						let n2s = match i2, fn2, ln2 with
						| Some i, Some fn, Some ln ->
							let n = Printf.sprintf "%s %s" fn ln in
							pcdata n
						| _ -> i [pcdata "no presenter"] in
							tr [td [pcdata (string_of_int lw)]; td [pcdata (Printer.Date.sprint "%d %b" day)];
								td [n1s]; td [n2s]]
					) slots (Moab_db.drop (start_week-1) weeks))
				)
			]
		)
		(function
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Eliom_registration.Any.register ~service:generate_schedule_service generate_schedule_page;
;;
