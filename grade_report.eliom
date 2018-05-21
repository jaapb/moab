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

let do_grade_report_page student_id (pub, (qg, (qf, (idg, (idf, (cg, cf)))))) =
	Lwt.catch (fun () ->
		let%lwt () = Moab_db.set_report_scores student_id !Moab.term pub qg qf idg idf cg cf in
		container [
			h1 [pcdata "Done"];
			p [pcdata (if pub then "Report graded and published." else "Report graded.")]
		]
	)
	(function
	| e -> error_page (Printexc.to_string e)
 	)
;;

let grade_report_page student_id () =
	let do_grade_report_service = create_attached_post ~fallback:grade_report_service
		~post_params:(bool "published" **
									int32 "quality_grade" ** string "quality_feedback" **
                  int32 "independence_grade" ** string "independence_feedback" ** 
									int32 "communication_grade" ** string "communication_feedback") () in
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		 ~service:do_grade_report_service do_grade_report_page;
	let%lwt u = Eliom_reference.get user in
	match u with
	| None -> Eliom_registration.Redirection.send (Eliom_registration.Redirection login_service)
	| Some (uid, _, _, is_admin) -> 
		Lwt.catch (fun () ->
			if not is_admin then
				container 
				[
					h1 [pcdata "Error"];
					p [pcdata "You must be an administrator to access this page"];
				]
			else
				let%lwt (is_pub, vqg, vqf, vig, vif, vcg, vcf) = Lwt.catch (fun () ->
				  Moab_db.get_report_scores ~unpublished:true student_id !Moab.term
				)
				(function
				| Not_found -> Lwt.return (false, 0l, "", 0l, "", 0l, "")
				| e -> Lwt.fail e) in
				container
				[
					h1 [pcdata "Report feedback"];
					Form.post_form ~service:do_grade_report_service (fun (pub, (qg, (qf, (idg, (idf, (cg, cf)))))) -> [
						table ~a:[a_class ["report_table"]] [
							tr [
								td ~a:[a_colspan 2] [
									Form.bool_checkbox_one ~checked:is_pub ~name:pub ();
									pcdata " Publish scores"
								]
							];
							tr [
								th [pcdata "Quality grade (max 10):"];
								td [Form.input ~input_type:`Number ~name:qg ~value:vqg Form.int32];
							];
							tr [
								th ~a:[a_colspan 2] [pcdata "Quality feedback:"]
							];
							tr [
								td ~a:[a_colspan 2] [
									Form.textarea ~a:[a_rows 10; a_cols 80] ~name:qf ~value:vqf ()
								]
							];
							tr [
								th [pcdata "Independence grade (max 10):"];
								td [Form.input ~input_type:`Number ~name:idg ~value:vig Form.int32];
							];
							tr [
								th ~a:[a_colspan 2] [pcdata "Independence feedback:"]
							];
							tr [
								td ~a:[a_colspan 2] [
									Form.textarea ~a:[a_rows 10; a_cols 80] ~name:idf ~value:vif ()
								]
							];
							tr [
								th [pcdata "Communicaiton grade (max 20):"];
								td [Form.input ~input_type:`Number ~name:cg ~value:vcg Form.int32];
							];
							tr [
								th ~a:[a_colspan 2] [pcdata "Communication feedback:"]
							];
							tr [
								td ~a:[a_colspan 2] [
									Form.textarea ~a:[a_rows 10; a_cols 80] ~name:cf ~value:vcf ()
								]
							];
							tr [
								td ~a:[a_colspan 2] [
									Form.input ~input_type:`Submit ~value:"Submit" Form.string
								]
							]
						]
					]) student_id
				]
		)
		(function
		| Failure s -> 
			container
			[
				h1 [pcdata "Error"];
				p [pcdata (Printf.sprintf "There was an inconsistency in the database (message: %s)." s)]
			]
		| e -> error_page (Printexc.to_string e))
;;

let () =
  Eliom_registration.Any.register ~service:grade_report_service grade_report_page;
;;
