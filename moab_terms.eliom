[%%shared
	open Eliom_content.Html
	open Eliom_content.Html.F
	open Eliom_parameter
	open CalendarLib
]

(* Local services *)

let%server setup_terms_action =
	Eliom_service.create_attached_post
		~fallback:Moab_services.setup_terms_service
		~post_params:(string "new_ayear" ** string "start1" ** string "end1" ** string "start2" ** string "end2" ** string "start3" ** string "end3")
		()

let%client setup_terms_action =
	~%setup_terms_action

(* Database access *)

let%server get_academic_years () =
	Moab_term_db.get_academic_years ()

let%client get_academic_years =
	~%(Eliom_client.server_function [%derive.json : unit]
			(Os_session.connected_wrapper get_academic_years))

let%server get_learning_weeks ayear =
	Moab_term_db.get_learning_weeks ayear

let%client get_learning_weeks =
	~%(Eliom_client.server_function [%derive.json : string]
			(Os_session.connected_wrapper get_learning_weeks))

let%server get_term_ids ayear =
	Moab_term_db.get_term_ids ayear

let%client get_term_ids =
	~%(Eliom_client.server_function [%derive.json : string]
			(Os_session.connected_wrapper get_term_ids))

(* Utility functions and widgets *)

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

let%shared academic_year_select_widget param =
	let ayear_opt t =
		D.Form.Option ([], t, None, false) in
	let%lwt ayears = get_academic_years () in
	match ayears with
	| [] -> Lwt.return (pcdata [%i18n S.no_academic_years_yet])
	| h::t ->	begin
		match param with
		| `Param p ->  Lwt.return @@ D.Form.select ~name:p Form.string (ayear_opt h) (List.map ayear_opt t)
		| `String s -> Lwt.return @@ D.Raw.select ~a:[a_name s] (List.map (fun x -> option (pcdata x)) (h::t))
		end

(* Handlers *)

let%server do_setup_terms () (new_ayear, (s1, (e1, (s2, (e2, (s3, e3)))))) =
	(* date return: YYYY-MM-DD *)
	let%lwt () = Lwt_list.iter_s (fun (s, e) ->
		try
			let sd = Printer.Date.from_fstring "%Y-%m-%d" s
			and ed = Printer.Date.from_fstring "%Y-%m-%d" e in
			Moab_term_db.add_term new_ayear (Date.year sd) (Date.week sd) (Date.week ed)
		with
			Invalid_argument _ -> Lwt.return_unit)
	[s1, e1; s2, e2; s3, e3] in
	Eliom_registration.Redirection.send (Eliom_registration.Redirection Os_services.main_service)

let%shared real_setup_terms_handler myid () () =
	let%lwt ayears = get_academic_years () in
	Moab_container.page (Some myid)
	[
		div ~a:[a_class ["content-box"]]
		[
			h1 [pcdata [%i18n S.setup_terms]];
			Form.post_form ~service:setup_terms_action (fun (new_ayear, (s1, (e1, (s2, (e2, (s3, e3)))))) -> [
				table [
					tr [
						th ~a:[a_colspan 2] [
							pcdata [%i18n S.add_academic_year];
							Form.input ~name:new_ayear ~input_type:`Text Form.string
						]
					];
					tr
					[
						td [pcdata [%i18n S.from]; pcdata " "; Form.input ~name:s1 ~input_type:`Date Form.string];
						td [pcdata [%i18n S.until]; pcdata " "; Form.input ~name:e1 ~input_type:`Date Form.string]
					];
					tr
					[
						td [pcdata [%i18n S.from]; pcdata " "; Form.input ~name:s2 ~input_type:`Date Form.string];
						td [pcdata [%i18n S.until]; pcdata " "; Form.input ~name:e2 ~input_type:`Date Form.string]
					];
					tr
					[
						td [pcdata [%i18n S.from]; pcdata " "; Form.input ~name:s3 ~input_type:`Date Form.string];
						td [pcdata [%i18n S.until]; pcdata " "; Form.input ~name:e3 ~input_type:`Date Form.string]
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
