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

let do_generate_report () (from_week, to_week) =
	let (tmpnam, out_ch) = Filename.open_temp_file "moab_report" ".csv" in
	let csv_ch = Csv.to_channel out_ch in
	let%lwt planned = Moab_db.get_planned_sessions !Moab.term in
	let%lwt csv = Lwt_list.mapi_s (fun n (year, week, sessions) ->
		match year, week, sessions with
		| Some y, Some w, Some s -> 
				let wk = Int32.to_int w in
				let lw = n + 1 in
				if (lw >= from_week) && (lw <= to_week) then
					let (sw, _) = Date.week_first_last wk y in
					Ocsigen_messages.console (fun () -> (Printf.sprintf "Learning week %d" lw));
					let%lwt	users = Moab_db.get_user_attendance !Moab.term lw in 	
					Lwt_list.map_s (fun (uid, p, x) ->
						Ocsigen_messages.console (fun () -> (Printf.sprintf "- uid %s" uid));
						let student_id = match p with None -> "" | Some q -> q in
						let nr_sessions = match x with None -> 0L | Some y -> y in
						Lwt.return [string_of_int lw;
						Int64.to_string s;
						student_id;
						Int64.to_string nr_sessions;
						Printer.Date.sprint "%Y-%m-%d" sw;
						"";
						uid;
						"";
						"";
						(Printf.sprintf "%s@live.mdx.ac.uk" uid);
						"";
						""	
						]	
					) users
				else
					Lwt.return []
		| _, _, _ -> Lwt.return []
	) planned in
	let csv_header = ["Week number"; "Scheduled sessions"; "Student Number"; "Sessions attended"; "Week starting"; "Tutor"; "Network Name"; "First Name"; "Last Name"; "Email"; "Visa?"; "Foundation?"] in
		Csv.output_all csv_ch (csv_header::List.flatten csv);
		Csv.close_out csv_ch;
		Eliom_registration.File.send ~content_type:"text/csv" tmpnam
;;

let attendance_report_page () () =
	let generate_report_service = create ~path:(Path ["attendance_report.csv"])
		~meth:(Post (unit, int "from_week" ** int "to_week")) () in
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:generate_report_service do_generate_report;
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, _, _) -> 
		Lwt.catch (fun () ->
			let%lwt x = Moab_db.last_learning_week 1 !Moab.term in
			let llw = match x with
			| None -> 25
			| Some y -> y in
			let fw = max 1 (llw - 1) in
			let tw = max 1 (llw - 5) in
			container [
				h1 [pcdata "Weekly attendance report"];
				Form.post_form ~service:generate_report_service 
				(fun (from_week, to_week) -> [
					table
					[
						tr [
							th [pcdata "From learning week: "];
							td [Form.input ~input_type:`Text ~name:from_week ~value:fw Form.int]
						];
						tr [
							th [pcdata "To learning week: "];
							td [Form.input ~input_type:`Text ~name:to_week ~value:tw Form.int]
						];
						tr [
							td ~a:[a_colspan 2] [Form.input ~input_type:`Submit ~value:"Generate" Form.string]
						]
					]
				]) ()
			]
		)
		(function
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Eliom_registration.Any.register ~service:attendance_report_service attendance_report_page;
;;
