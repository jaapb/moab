let default d opt =
	match opt with
	| None -> d
	| Some x -> x
;;

let optmap f opt =
	match opt with
	| None -> None
	| Some x -> Some (f x)
;;
