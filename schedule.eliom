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

let schedule_page () () =
	(*let do_attendance_service = create_attached_post ~fallback:attendance_service
		~post_params:(string "user_id" ** int32 "session_id") () in
	Moab_app.register ~scope:Eliom_common.default_session_scope
		~service:do_attendance_service do_attendance_page; *)
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> container []
	| Some (uid, _, _) -> 
		Lwt.catch (fun () ->
			let term = !Moab.term in
			let start_week = !Moab.start_week in
			let%lwt group = Moab_db.get_user_group uid in
			let%lwt slots = Moab_db.get_presentation_slots term group in
			let%lwt x = Lwt_list.map_s (fun (y, wd) ->
				let w = Int32.to_int y in
				let%lwt (p1, p2) = Moab_db.get_presenters group w wd in
				Lwt.return (w, wd, p1, p2)	
			) slots in
			container
			[
				h1 [pcdata "Presentation schedule"];
				table
				(
					tr [th [pcdata "Week"]; th [pcdata "Date"]; th [pcdata "Presenter 1"]; th [pcdata "Presenter 2"]]::
					(List.flatten (List.mapi (fun n (w, wd, p1, p2) ->
						let lw = n + 1 in
						let (sd, _) = Date.week_first_last w (if w > 26 then term else term + 1) in
						let day = Date.add sd (Date.Period.day (wd - 1)) in
						let p1s = match p1 with
						| None -> i [pcdata "no presenter"]
						| Some x -> pcdata x in
						let p2s = match p2 with
						| None -> i [pcdata "no presenter"]
						| Some x -> pcdata x in
						if lw < start_week then
							[]
						else
							[tr [td [pcdata (string_of_int lw)]; td [pcdata (Printer.Date.sprint "%d %b" day)];
								td [p1s]; td [p2s]]]
					) x))
				)
			]
		)
		(function
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Moab_app.register ~service:schedule_service schedule_page;
;;
