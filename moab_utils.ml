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

let compute_with_deduction fg d =
	if d >= 25 then fg
	else if d >= 20 then fg *. 0.9
	else if d >= 15 then fg *. 0.8
	else if d >= 10 then fg *. 0.7
	else if d >= 5 then fg *. 0.6
	else fg *. 0.5
;;

let calculate_pres peer tutor duration feedback exempt: float option =
	if feedback < 75 && not exempt
	then
		None
	else
	match peer with
	| None -> None
	| Some p -> 
		match tutor with
		| None -> None
		| Some t -> Some (p +. (compute_with_deduction t duration))
;;

let calculate_project q i c: int32 option =
	match (q, i, c) with
	| Some x, Some y, Some z -> Some (Int32.add x (Int32.add y z))
	| _, _, _ -> None
;;

let calculate_total pres blog proj: float option =
	match pres, blog, proj with
	| Some x, y, Some z -> Some (x +. (float_of_int y) +. (Int32.to_float z))
	| _, _, _ -> None
;;

let twenty_point_grade x =
	match x with
	| None -> 20
	| Some y -> begin 
			if y >= 79.0 then 1
			else if y >= 76.0 then 2
			else if y >= 73.0 then 3
			else if y >= 70.0 then 4
			else if y >= 67.0 then 5
			else if y >= 65.0 then 6
			else if y >= 62.0 then 7
			else if y >= 60.0 then 8
			else if y >= 57.0 then 9
			else if y >= 55.0 then 10
			else if y >= 52.0 then 11
			else if y >= 50.0 then 12
			else if y >= 47.0 then 13
			else if y >= 45.0 then 14
			else if y >= 42.0 then 15
			else if y >= 40.0 then 16
			else if y >= 35.0 then 17
			else if y >= 30.0 then 18
			else 19
		end
;;

