[%%shared
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

let%server get_terms () =
	Moab_term_db.get_terms ()

let%client get_terms =
	~%(Eliom_client.server_function [%derive.json : unit]
			(Os_session.connected_wrapper get_terms))

let%server get_learning_weeks term =
	Moab_term_db.get_learning_weeks term

let%client get_learning_weeks =
	~%(Eliom_client.server_function [%derive.json : string]
			(Os_session.connected_wrapper get_learning_weeks))

let%shared learning_week_of_date t d =
	let res = ref None in
	let%lwt l = get_learning_weeks t in	
	let%lwt () = Lwt_list.iteri_s (fun i (w, y) ->
		if w = (Int32.of_int (Date.week d)) && y = (Date.year d) then
		begin
			res := (Some i)
		end;
		Lwt.return_unit
	) l in
	Lwt.return !res

let%shared date_of_learning_week t lw d =
	let%lwt l = get_learning_weeks t in
	let (w, y) = List.nth l (lw - 1) in
	let (s, e) = Date.week_first_last (Int32.to_int w) y in
	Lwt.return (Date.add s (Date.Period.day (Date.int_of_day d - 1)))

let%server setup_terms_action =
	Eliom_service.create_attached_post
		~fallback:Moab_services.setup_terms_service
		~post_params:(string "new_term" ** string "start1" ** string "end1" ** string "start2" ** string "end2" ** string "start3" ** string "end3")
		()

let%client setup_terms_action =
	~%setup_terms_action

let%server do_setup_terms () (new_term, (s1, (e1, (s2, (e2, (s3, e3)))))) =
	(* date return: YYYY-MM-DD *)
	let%lwt () = Lwt_list.iter_s (fun (s, e) ->
		try
			let sd = Printer.Date.from_fstring "%Y-%m-%d" s
			and ed = Printer.Date.from_fstring "%Y-%m-%d" e in
			Moab_term_db.add_term new_term (Date.year sd) (Date.week sd) (Date.week ed)
		with
			Invalid_argument _ -> Lwt.return_unit)
	[s1, e1; s2, e2; s3, e3] in
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared real_setup_terms_handler myid () () =
	let%lwt terms = get_terms () in
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]]
		[
			h1 [pcdata "Term setup"];
			Form.post_form ~service:setup_terms_action (fun (new_term, (s1, (e1, (s2, (e2, (s3, e3)))))) -> [
				table [
					tr [
						th ~a:[a_colspan 2] [
							pcdata "Add term ";
							Form.input ~name:new_term ~input_type:`Text Form.string
						]
					];
					tr
					[
						td [pcdata "From "; Form.input ~name:s1 ~input_type:`Date Form.string];
						td [pcdata "To "; Form.input ~name:e1 ~input_type:`Date Form.string]
					];
					tr
					[
						td [pcdata "From "; Form.input ~name:s2 ~input_type:`Date Form.string];
						td [pcdata "To "; Form.input ~name:e2 ~input_type:`Date Form.string]
					];
					tr
					[
						td [pcdata "From "; Form.input ~name:s3 ~input_type:`Date Form.string];
						td [pcdata "To "; Form.input ~name:e3 ~input_type:`Date Form.string]
					];
					tr
					[ 
						td ~a:[a_colspan 2] [Form.input ~a:[a_class ["button"]] ~input_type:`Submit ~value:"Save" Form.string]
					]
				]
			]) ()
		]
	]

let%server setup_terms_handler myid () () =
	Eliom_registration.Any.register ~scope:Eliom_common.default_session_scope
		~service:setup_terms_action do_setup_terms;
	real_setup_terms_handler myid () ()

let%client setup_terms_handler =
	real_setup_terms_handler
