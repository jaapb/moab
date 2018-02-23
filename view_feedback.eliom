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

let view_feedback_page () () =
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, _, _, is_admin) -> 
		Lwt.catch (fun () ->
			let%lwt scores = Moab_db.get_presentation_averages uid !term in
			let total = List.fold_left (fun acc (_, _, s) -> acc +. (float_of_string (Moab_utils.default "" s))) 0.0 scores in
			let%lwt comments = Moab_db.get_presentation_comments uid !term in
			let%lwt (topic, duration, pgrade, tutor_comments) = Moab_db.get_presentation_tutor_feedback uid !term in
			container
			[
				h1 [pcdata "View presentation feedback"];	
				p [pcdata "Your topic: "; pcdata topic];
				h2 [pcdata "Peer mark"];
				p [pcdata "This is the average of all scores given by your fellow students, plus the tutor. Each score counts equally. Scores range from 0 to 5; the five averages are added together to give a final presentation score from 0 to 25."];
				table (
					tr [
						th [pcdata "Criterion"];
						th [pcdata "Average"];
					]::
					List.rev (tr [
						td [em [pcdata "Total score"]];
						td [em [pcdata (Printf.sprintf "%.1f" total)]]
					]::
					List.rev_map (fun (_, crit, score) ->
						tr [
							td [pcdata (Moab_utils.default "" crit)];
							td [pcdata (Moab_utils.default "" score)]
						]	
					) scores)
				);
				h3 [pcdata "Peer comments"];
				table	~a:[a_class ["peer_comments"]] (
					List.flatten (List.map (fun (cname, ccl) ->
						match ccl with
						| [] -> [tr [th [pcdata cname]; td [pcdata "no comments"]]]
						| h::t -> 
								tr [
									th ~a:[a_rowspan (List.length ccl)] [pcdata cname];
									td [pcdata h]
								]::
								List.map (fun x -> tr [td [pcdata x]]) t
					) (List.rev comments))
				);
				h2 [pcdata "Tutor mark"];
				p [strong [pcdata "Please note"]; pcdata " that these marks are provisional and should only be taken as an indication. Final marks will only be awarded after all presentations have been given (to ensure fairness)."];
				p [pcdata "Grade bands:"];
				ul [
					li [pcdata "1: 70-100%, 17.5-25 points"];
					li [pcdata "U2: 60-70%, 15-17.5 points"];
					li [pcdata "L2: 50-60%, 12.5-15 points"];
					li [pcdata "3: 40-50%, 10-12.5 points"];
					li [pcdata "F: 0-40%, 0-10 points"]
				];
				p [pcdata "For duration, the deduction (which is from the tutor mark only) is as follows:"];
				ul [
					li [pcdata "25 minutes or more: no deduction"];
					li [pcdata "20-24 minutes: 10% deduction"];
					li [pcdata "15-19 minutes: 20% deduction"];
					li [pcdata "10-14 minutes: 30% deduction"];
					li [pcdata "5-9 minutes: 40% deduction"];
					li [pcdata "0-4 minutes: 50% deduction"]
				];
				table ~a:[a_class ["tutor_comments"]] [
					tr [
						th [pcdata "Provisional grade: "];
						td [pcdata pgrade]
					];
					tr [
						th [pcdata "Duration: "];
						td [pcdata (Printf.sprintf "%d minutes" duration)]
					];
					tr [
						th [pcdata "Tutor comments: "];
						td [pre [pcdata tutor_comments]]
					];
				]

			]
		)
		(function
		| Moab_db.No_group -> error_page "You are an administrator."
		| Not_found -> error_page "You have not yet given your presentation."
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Eliom_registration.Any.register ~service:view_feedback_service view_feedback_page;
;;
