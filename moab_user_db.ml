open Os_db
open Lwt

let update_password userid password =
	if password = "" then fail_with "empty password"
	else
		full_transaction_block (fun dbh ->
			PGSQL(dbh) "UPDATE ocsigen_start.users \
				SET password = crypt($password, gen_salt('bf')) \
				WHERE userid = $userid"
		)

let verify_password email password =
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

let find_user email =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT userid \
			FROM ocsigen_start.users \
			WHERE main_email = $email") >>=
	function
	| [x] -> return x
	| [] -> fail Not_found
	| _ -> fail (Invalid_argument "find_user found multiple users")

let get_user_type userid =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "SELECT user_type \
			FROM ocsigen_start.users \
			WHERE userid = $userid") >>=
	function
	| [x] -> return x
	| _ -> fail No_such_resource

let add_user user_type fn ln email password =
	full_transaction_block (fun dbh ->
		PGSQL(dbh) "INSERT INTO ocsigen_start.users \
			(firstname, lastname, main_email, user_type) \
			VALUES \
			($fn, $ln, $email, $user_type)
			RETURNING userid" >>=
	function
	| [x] -> (match password with
			| None -> Lwt.return_unit
			| Some p ->	update_password x p) >>=
		fun () -> Lwt.return x
	| _ -> fail (Invalid_argument "add_user returned no rows"))

let get_name uid =
	full_transaction_block (fun dbh -> PGSQL(dbh)
		"SELECT firstname, lastname \
			FROM ocsigen_start.users \
			WHERE userid = $uid") >>=
	function
	| [] -> fail Not_found
	| [x] -> return x
	| _ -> fail (Invalid_argument "get_name found multiple users with same uid")
