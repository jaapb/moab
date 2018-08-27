open Os_db
open Lwt

let update_password userid password =
	Ocsigen_messages.console (fun () -> "[update_password]");
	if password = "" then fail_with "empty password"
	else
		full_transaction_block (fun dbh ->
			PGSQL(dbh) "UPDATE ocsigen_start.users \
				SET password = crypt($password, gen_salt('bf')) \
				WHERE userid = $userid"
		)

let verify_password email password =
	Ocsigen_messages.console (fun () -> "[verify_password]");
	if password = ""
	then fail No_such_resource
	else
		full_transaction_block (fun dbh ->
			PGSQL(dbh) "SELECT userid \
				FROM ocsigen_start.users \
				WHERE password = crypt($password,password) AND main_email = $email"
		) >>=
		function
		| [uid] -> return uid
		| _ -> fail No_such_resource

