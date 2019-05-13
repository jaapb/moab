(* This file was generated by Ocsigen Start.
   Feel free to use it, modify it, and redistribute it as you wish. *)

let%server () =
	Os_db.pwd_crypt_ref :=
		((fun password -> Bcrypt.string_of_hash (Bcrypt.hash ~variant:Bcrypt.A password)),
		 (fun _ password1 password2 ->
				Bcrypt.verify password1 (Bcrypt.hash_of_string password2)))

let%shared () =
  Eliom_registration.Action.register
    ~service:Os_services.set_personal_data_service
    Moab_handlers.set_personal_data_handler;

  Eliom_registration.Redirection.register
    ~service:Os_services.set_password_service
    Moab_handlers.set_password_handler;

  Eliom_registration.Action.register
    ~service:Os_services.forgot_password_service
    Moab_handlers.forgot_password_handler;

  Eliom_registration.Action.register
    ~service:Os_services.preregister_service
    Moab_handlers.preregister_handler;

  Eliom_registration.Action.register
    ~service:Os_services.sign_up_service
    Os_handlers.sign_up_handler;

  Eliom_registration.Action.register
    ~service:Moab_services.connect_service
    Moab_handlers.connect_handler;

  Eliom_registration.Unit.register
    ~service:Os_services.disconnect_service
    (Os_handlers.disconnect_handler ~main_page:true);

  Eliom_registration.Any.register
    ~service:Os_services.action_link_service
    (Os_session.Opt.connected_fun
       Moab_handlers.action_link_handler);

  Eliom_registration.Action.register
    ~service:Os_services.add_email_service
    Os_handlers.add_email_handler;

  Eliom_registration.Action.register
    ~service:Os_services.update_language_service
    Moab_handlers.update_language_handler;

  Moab_base.App.register
    ~service:Os_services.main_service
    (Moab_page.Opt.connected_page Moab_handlers.main_service_handler);

  Moab_base.App.register
    ~service:Moab_services.settings_service
    (Moab_page.Opt.connected_page Moab_handlers.settings_handler);

	Moab_base.App.register
		~service:Moab_services.add_students_service
		(Moab_page.connected_page Moab_students.add_students_handler);

	Moab_base.App.register
		~service:Moab_services.setup_terms_service
		(Moab_page.connected_page Moab_terms.setup_terms_handler);

	Moab_base.App.register
		~service:Moab_services.setup_sessions_service
		(Moab_page.connected_page Moab_sessions.setup_sessions_handler);

	Moab_base.App.register
		~service:Moab_services.edit_blog_service
		(Moab_page.connected_page Moab_blogs.edit_blog_handler);

	Moab_base.App.register
		~service:Moab_services.show_blog_service
		(Moab_page.connected_page Moab_blogs.show_blog_handler);

	Moab_base.App.register
		~service:Moab_services.register_attendance_service
		(Moab_page.connected_page Moab_attendance.register_attendance_handler);

	Moab_base.App.register
		~service:Moab_services.schedule_presentation_service
		(Moab_page.connected_page Moab_presentations.schedule_presentation_handler);

	Moab_base.App.register
		~service:Moab_services.generate_attendance_report_service
		(Moab_page.connected_page Moab_attendance.generate_report_handler);

	Moab_base.App.register
		~service:Moab_services.view_schedule_service
		(Moab_page.connected_page Moab_presentations.view_schedule_handler);

	Moab_base.App.register
		~service:Moab_services.submit_report_service
		(Moab_page.connected_page Moab_reports.submit_report_handler);

	Moab_base.App.register
		~service:Moab_services.presentation_feedback_service
		(Moab_page.connected_page Moab_presentations.presentation_feedback_handler);

	Moab_base.App.register
		~service:Moab_services.view_feedback_service
		(Moab_page.connected_page Moab_presentations.view_feedback_handler);

	Moab_base.App.register
		~service:Moab_services.view_grades_service
		(Moab_page.connected_page Moab_grades.view_grades_handler)

let%shared () =
	CalendarLib.Printer.day_name :=
	(function
	| Sun -> [%i18n S.sunday]
	| Mon -> [%i18n S.monday]
	| Tue -> [%i18n S.tuesday]
	| Wed -> [%i18n S.wednesday]
	| Thu -> [%i18n S.thursday]
	| Fri -> [%i18n S.friday]
	| Sat -> [%i18n S.saturday]);
	CalendarLib.Printer.month_name :=
	(function
	| Jan -> [%i18n S.january]
	| Feb -> [%i18n S.february]
	| Mar -> [%i18n S.march]
	| Apr -> [%i18n S.april]
	| May -> [%i18n S.may]
	| Jun -> [%i18n S.june]
	| Jul -> [%i18n S.july]
	| Aug -> [%i18n S.august]
	| Sep -> [%i18n S.september]
	| Oct -> [%i18n S.october]
	| Nov -> [%i18n S.november]
	| Dec -> [%i18n S.december])

