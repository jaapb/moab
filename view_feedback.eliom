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
				p [pcdata "Your topic: "; pcdata (Moab_utils.default "<not recorded>" topic)];
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
				table [
					tr [
						th [pcdata "Provisional grade: "];
						td [pcdata (Moab_utils.default "<not recorded>" pgrade)]
					];
					tr [
						th [pcdata "Duration: "];
						td [pcdata (match duration with | None -> "<not recorded>" | Some d -> Printf.sprintf "%d minutes" d)]
					];
					tr [
						th [pcdata "Tutor comments: "];
						td [pcdata tutor_comments]
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
