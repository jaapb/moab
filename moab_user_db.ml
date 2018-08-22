let verify_password userid password =
	if password = ""
	then Lwt.fail Os_db.No_such_resource
	else
		Lwt.return 0L
