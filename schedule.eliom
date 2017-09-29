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

let schedule_err = Eliom_reference.eref ~scope:Eliom_common.request_scope None;;

let add_schedule_action () (user_id, (term, (group, week))) =
	Lwt.catch (fun () -> let%lwt (p1, p2) = Moab_db.get_presenters term group week in
		match (p1, p2) with
		| None, None -> Moab_db.set_presenter user_id term group week true
		| Some i1, None -> if i1 = user_id then Eliom_reference.set schedule_err (Some "You are already signed up for this session.") else Moab_db.set_presenter user_id term group week false
		| None, Some i2 -> if i2 = user_id then Eliom_reference.set schedule_err (Some "You are already signed up for this session.") else Moab_db.set_presenter user_id term group week true
		| Some i1, Some i2 -> if i1 = user_id || i2 = user_id then Eliom_reference.set schedule_err (Some "You are already signed up for this session.") else Eliom_reference.set schedule_err (Some "That session is full.")
	)
	(function
	| e -> Eliom_reference.set schedule_err (Some (Printexc.to_string e))
	)
;;

let schedule_page () () =
	let is_mine p w =
		match p with
		| None -> false
		| Some (w', _) -> w = w' in
	let add_schedule_service = create_attached_post ~fallback:schedule_service
		~post_params:(string "user_id" ** int "term" ** int "group" ** int "week") () in
	Eliom_registration.Action.register ~scope:Eliom_common.default_session_scope
		~service:add_schedule_service add_schedule_action;
	let%lwt u = Eliom_reference.get user in
	let%lwt err = Eliom_reference.get schedule_err in
	Eliom_reference.set schedule_err None;
	match u with
	| None -> container []
	| Some (uid, _, _) -> 
		Lwt.catch (fun () ->
			let term = !Moab.term in
			let start_week = !Moab.start_week in
			let%lwt (group, wd) = Moab_db.get_user_group uid term in
			let%lwt slots = Moab_db.get_presentation_slots term group start_week in
			let%lwt my_pres = Moab_db.get_presentation_week uid term in
			container
			[
				h1 [pcdata "Presentation schedule"];
				table
				(
					tr [th [pcdata "Week"]; th [pcdata "Date"]; th [pcdata "Presenter 1"]; th [pcdata "Presenter 2"]]::
					(List.mapi (fun n (w, i1, n1, i2, n2) ->
						let lw = n + start_week in
						let (sd, _) = Date.week_first_last w (if w > 26 then term else term + 1) in
						let day = Date.add sd (Date.Period.day (wd - 1)) in
						let n1s = match i1, n1 with
						| Some i, Some n -> if i = uid then b [pcdata n] else pcdata n
						| _ -> i [pcdata "no presenter"] in
						let n2s = match i2, n2 with
						| Some i, Some n -> if i = uid then b [pcdata n] else pcdata n
						| _ -> i [pcdata "no presenter"] in
							tr [td [pcdata (string_of_int lw)]; td [pcdata (Printer.Date.sprint "%d %b" day)];
								td [n1s]; td [n2s]]
					) slots)
				);
				h2 [pcdata "Schedule your presentation"];
				p [match err with
				| None -> pcdata "Choose a week for your presentation."
				| Some e -> pcdata e];
				Form.post_form ~service:add_schedule_service
				(fun (user_id, (t, (g, week))) -> [
					table
					[
						tr [
							td [pcdata "Week:"];
							td [match slots with
								| [] -> Form.input ~input_type:`Text ~name:week Form.int
								| (w, _, _, _, _)::t -> Form.select ~name:week Form.int
									(Form.Option ([], w, Some (pcdata (string_of_int start_week)), is_mine my_pres w))
									(List.mapi (fun n (w, _, _, _, _) ->
										Form.Option ([], w, Some (pcdata (string_of_int (n+start_week+1))), is_mine my_pres w)
									) t)
							]
						];
						tr [
							td [
								Form.input ~input_type:`Hidden ~name:user_id ~value:uid Form.string;
								Form.input ~input_type:`Hidden ~name:t ~value:term Form.int;
								Form.input ~input_type:`Hidden ~name:g ~value:group Form.int;
								Form.input ~input_type:`Submit ~value:"Save" Form.string
							]
						]
					]
				]) ();
				p [a ~service:main_service [pcdata "Return to main menu"] ()]
			]
		)
		(function
		| Moab_db.No_group -> error_page "you are an administrator"
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Moab_app.register ~service:schedule_service schedule_page;
;;
