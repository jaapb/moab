(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

[%%shared.start]

val settings_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val os_github_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.ext,
    Eliom_service.non_reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val ocsigen_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.ext,
    Eliom_service.non_reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val connect_service:
	(
		unit,
		(string * string) * bool,
    Eliom_service.post,
    Eliom_service.non_att,
    Eliom_service.co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
    ([ `One of string ] Eliom_parameter.param_name *
			[ `One of string ] Eliom_parameter.param_name) *
			[ `One of bool ] Eliom_parameter.param_name,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val add_students_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val setup_terms_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val setup_sessions_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val edit_blog_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val show_blog_service :
  (
    int64 option * int option,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithSuffix ],
    [ `One of int64 ] Eliom_parameter.param_name *
		[ `One of int ] Eliom_parameter.param_name,
    unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val register_attendance_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val schedule_presentation_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
    unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val generate_attendance_report_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
		unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val view_schedule_service :
  (
    int,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithSuffix ],
    [ `One of int ] Eliom_parameter.param_name,
		unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val submit_report_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
		unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val presentation_feedback_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
		unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val view_presentation_feedback_service :
  (
    int64 option,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithSuffix ],
    [ `One of int64 ] Eliom_parameter.param_name,
    unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val view_grades_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
		unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t

val report_feedback_service :
  (
    unit,
    unit,
    Eliom_service.get,
    Eliom_service.att,
    Eliom_service.non_co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
		unit,
    Eliom_service.non_ocaml
  ) Eliom_service.t
